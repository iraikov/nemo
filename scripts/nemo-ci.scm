

(use matchable data-structures posix files tcp srfi-1 regex setup-api uri-generic awful sendfile)
(require-library spiffy intarweb)
(import (only spiffy current-request current-response write-logged-response 
	      with-headers mime-type-map root-path file-extension->mime-type)
	(only intarweb response-port request-method  request-headers
	      header-values header-value etag-matches? response-has-message-body-for-request?
	      ))

(enable-sxml #t)


(define v:quiet 0)
(define v:info  1)
(define v:debug 2)

(define verbose (make-parameter v:info))

(define-constant +default-tcp-connect-timeout+ 10000) ; 10 seconds
(define-constant +default-tcp-read/write-timeout+ 20000) ; 20 seconds

(tcp-connect-timeout +default-tcp-connect-timeout+)
(tcp-read-timeout +default-tcp-read/write-timeout+)
(tcp-write-timeout +default-tcp-read/write-timeout+)

(define http-user-agent "build-nemo")

(define chicken-home "/home/igr/bin/chicken")
(define build-location-prefix "/home/igr/build/nemo") 
(define (build-location version) (make-pathname build-location-prefix (sprintf "nemo-~A" version)))
(define (build-log-path version) (make-pathname build-location-prefix (string-append "log." version)) )
(define (build-lock-path version) (make-pathname (build-location version) (string-append "lock." version)) )

(define release-dir build-location-prefix)
(define (release-file-path version) (make-pathname release-dir (sprintf "nemo-~A.tar.gz" version)))
(define (download-file-name version) (pathname-strip-directory (release-file-path version)))

(debug-file (make-pathname build-location-prefix "/debug.log"))
;;(error-log (make-pathname build-location-prefix "/debug.log"))


(define henrietta-server "code.call-cc.org")
(define henrietta-cgi "/cgi-bin/henrietta.cgi")
(define (henrietta-listversions-uri name)
  (uri-reference
   (sprintf "http://~A/~A?~A" 
	    henrietta-server 
	    henrietta-cgi 
	    (sprintf "name=~A&listversions=1" name))))


(define (sed-quote str)
  (let ((lst (string->list str)))
    (let recur ((lst lst) (ax '()))
      (if (null? lst) (list->string (reverse ax))
	  (let ((c (car lst)))
	    (if (char=? c #\/) (recur (cdr lst) (cons c (cons #\\ ax)))
		(recur (cdr lst) (cons c ax))))
	  ))))


(define (quotewrap str)
  (cond ((quotewrapped? str) str)
	((string-any char-whitespace? str)
	 (string-append "\"" str "\""))
	(else str)))


(define (d fstr . args)
  (if (= (verbose)  v:debug)
      (let ([port (current-output-port)])
	(apply fprintf port fstr args)
	(flush-output port) ) ))


(define (info fstr . args)
  (if (>= (verbose) v:info)
      (let ([port (current-output-port)])
	(apply fprintf port fstr args)
	(flush-output port) ) ))


(define (run:execute explist)
  (define (smooth lst)
    (let ((slst (map ->string lst)))
      (string-intersperse (cons (car slst) (cdr slst)) " ")))
  (for-each (lambda (cmd)
	      (info "  ~A~%~" cmd)
	      (system (->string cmd)))
	    (map smooth explist)))


(define (run:execute* explist)
  (define (smooth lst)
    (let ((slst (map ->string lst)))
      (string-intersperse (cons (car slst) (cdr slst)) " ")))
  (for-each (lambda (cmd)
	      (info "  ~A~%~" cmd)
	      (system* "~a" cmd))
	    (map smooth explist)))



(define-syntax run
  (syntax-rules ()
    ((_ exp ...)
     (run:execute* (list `exp ...)))))


(define-syntax run-
  (syntax-rules ()
    ((_ exp ...)
     (run:execute (list `exp ...)))))


(define (ipipe:execute lam cmd)
  (define (smooth lst)
    (let ((slst (map ->string lst)))
      (string-intersperse (cons (car slst) (cdr slst)) " ")))
  ((lambda (cmd) 
     (info "  ~A~%~" cmd)
     (with-input-from-pipe (sprintf "~a" cmd) lam))
   (smooth cmd)))


(define-syntax ipipe
  (syntax-rules ()
    ((_ lam exp)
     (ipipe:execute lam `exp ))))


(define (network-failure msg . args)
  (signal
   (make-composite-condition
    (make-property-condition
       'exn
       'message "invalid response from server"
       'arguments args)
    (make-property-condition 'http-fetch))) )



(define (make-HTTP-GET/1.1 location user-agent host
			   #!key
			   (port 80)
			   (connection "close")
			   (accept "*")
			   (content-length 0))
  (conc
   "GET " location " HTTP/1.1" "\r\n"
   "Connection: " connection "\r\n"
   "User-Agent: " user-agent "\r\n"
   "Accept: " accept "\r\n"
   "Host: " host #\: port "\r\n"
   "Content-length: " content-length "\r\n"
   "\r\n") )


(define (match-http-response rsp)
  (and (string? rsp)
       (string-match "HTTP/[0-9.]+\\s+([0-9]+)\\s+.*" rsp)) )


(define (response-match-code? mrsp code)
  (and mrsp (string=? (number->string code) (cadr mrsp))) )


(define (match-chunked-transfer-encoding ln)
  (string-match "[Tt]ransfer-[Ee]ncoding:\\s*chunked.*" ln) )


(define (http-fetch-string uri)
  (d "fetching ~s ...~%" (uri->string uri))
  (match-let (((_ ((_ host port) ('/ . path) query) _) (uri->list uri)))
    (let* ((port      (or port 80))
	   (locn      (uri->string (update-uri (update-uri uri scheme: #f) host: #f)))
	   (query     (and query (not (string-null? query)) query)))
      (d "connecting to host ~s, port ~a ...~%" host port)
      (let-values ([(in out) (tcp-connect host port)])
	(d "requesting ~s ...~%" locn)
	(display
	 (make-HTTP-GET/1.1 locn http-user-agent host port: port accept: "*/*")
	 out)
	(flush-output out)
	(d "reading response ...~%")
	(let ([chunked #f] [ok-response #f])
	  (let* ([h1 (read-line in)]
		 [response-match (match-http-response h1)])
	    (d "~a~%" h1)
	    ;;*** handle redirects here
	    (cond ((response-match-code? response-match 200)
		   (set! ok-response #t))
		  ((response-match-code? response-match 404)
		   (d "file not found on server: ~s~%" locn))
		  (else (network-failure "invalid response from server" h1) ))
	    (and ok-response
		 (begin
		   (let loop ()
		     (let ([ln (read-line in)])
		       (unless (string-null? ln)
			 (when (match-chunked-transfer-encoding ln) (set! chunked #t))
			 (d "~a~%" ln)
			 (loop) ) ) )
		   (if chunked
		       (begin
			 (d "reading chunks ...~%")
			 (let ([data (read-chunks in)])
			   (close-input-port in)
			   (close-output-port out)
			   data ))
		       
		       (begin
			 (d "reading data ...~%")
			 (let ([data (read-string #f in)])
			   (close-input-port in)
			   (close-output-port out)
			   data ))
		       )
		   ))
	    ))
	))
    ))

(define (read-chunks in)
  (let get-chunks ([data '()])
    (let ([size (string->number (read-line in) 16)])
      (if (zero? size)
	  (string-concatenate-reverse data)
	  (let ([chunk (read-string size in)])
	    (read-line in)
	    (get-chunks (cons chunk data)) ) ) ) ) )


(define (call-with-input-file* file proc)
  (call-with-input-file
      file (lambda (p)
	     (handle-exceptions exn
		(begin (close-input-port p) (raise exn))
		(proc p)))))


(define (build build-dir local-version version lock-file log-file)
  (if (not (file-exists? lock-file))
      (with-output-to-file log-file
	(run (mkdir -p ,build-dir))
	(run (touch ,lock-file))
	(if (and (version>=? version local-version)
		 (not (version>=? local-version version)))
	    (run- (,(make-pathname chicken-home "/bin/chicken-install") nemo)))
	(run (,(make-pathname chicken-home "/bin/chicken-install")
	      "-deploy"  "-prefix" ,build-dir
	      make matchable iexpr sxml-transforms ssax sxpath datatype vector-lib  
	      digraph graph-bfs graph-cycles graph-scc mathh strictly-pretty varsubst 
	      silex lalr ersatz utf8 uri-generic defstruct iset
	      getopt-long dyn-vector input-parse nemo >> ,log-file))
	(run (cp ,(make-pathname chicken-home "/lib/libchicken.so*") ,build-dir))
	(run (mv ,(make-pathname build-dir "/bin/nemo") ,build-dir))
	(run (rm -rf ,(make-pathname build-dir "/bin")))
	(run (tar zcf ,(release-file-path version)  ,build-dir 
		  -P --transform ,(sprintf "\"s/~A//\"" (sed-quote build-location-prefix))  ))
	(run (rm ,lock-file))
	)))


(define (update-nemo-build)
  (let ((local-version
	 (car 
	  (alist-ref 'version
		     (let ((v (extension-information 'nemo)))
		       (if (not v)
			   (if (zero? (run- (,(make-pathname chicken-home "/bin/chicken-install") nemo)))
			       (extension-information 'nemo)
			       (error 'build-name "unable to obtain currently installed version"))
			   v
			   ))
		     )))
	(remote-version (car (sort (string-split (http-fetch-string
						  (henrietta-listversions-uri 'nemo)) "\n")
				   version>=? )))
	)
    (let ((loc (build-location remote-version)))
      (if (or (not (file-exists? loc)) 
	      (not (file-exists? (release-file-path remote-version))))
	  (let ((lock-file (build-lock-path remote-version))
		(log-file (build-log-path remote-version)))
	    (process-fork (lambda () (build loc local-version remote-version lock-file log-file)))))
      (list remote-version loc))
    ))


(mime-type-map
 (cons '("gz" . application/x-gzip)
       (mime-type-map)))


;; From spiffy
(define (call-with-input-file* file proc)
  (call-with-input-file
      file (lambda (p)
	     (handle-exceptions exn
		(begin (close-input-port p) (raise exn))
		(proc p)))))


;; Also from spiffy (send-static-file renamed to send-file)
(define (send-file filename)
  (condition-case
   (let* ((path (make-pathname (root-path) filename))
          (h (request-headers (current-request)))
          (size (file-size path))
          (last-modified (file-modification-time path))
          (etag (cons 'strong (conc size "-" last-modified)))
          (unmodified
           (or (and-let* ((t (header-values 'if-none-match h)))
                 (etag-matches? etag t))
               (and-let* ((t (header-value 'if-modified-since h)))
                 (<= last-modified (utc-time->seconds t))))))
     (parameterize
         ((current-response
           (if unmodified
               (update-response (current-response) status: 'not-modified)
               (current-response))))
       (with-headers
        `((last-modified #(,(seconds->utc-time last-modified)))
          (etag ,etag)
          (content-length ,(if unmodified 0 size))
          (content-type ,(file-extension->mime-type
                          (pathname-extension filename)))
          (content-disposition
           #(attachment
             ((filename . ,filename)))))
        (lambda ()
          (write-logged-response)
          (when ((response-has-message-body-for-request?)
                 (current-response) (current-request))
            (call-with-input-file*
             path (lambda (f) (sendfile f (response-port (current-response)))))
	    ))
	)))
   ((exn i/o file) (send-status 'forbidden))))



	
(define-page "/nemo"
  (lambda ()
    (let ((version.path (update-nemo-build))
	  (sys (system-information)))
      `((h1 "NEMO")
	(p)
	(p ,(sprintf "The current version of NEMO is ~A~%" (car version.path)))
	(p ,(link "/nemo-download" (sprintf "Download NEMO version ~A binary for ~A ~A~%" 
					    (car version.path) (list-ref sys 0) (list-ref sys 4))))
	(p ,(link "/nemo-build-log" (sprintf "Check the build log of NEMO binary version ~A~%" 
					     (car version.path))))
	))))
	


(define-page "/nemo-build-log"
  (lambda () 
    (let ((version.path (update-nemo-build)))
      `(pre . ,(intersperse (read-lines (build-log-path (car version.path))) "\n"))
      )))


(define-page "/nemo-download"
  (lambda ()
    (let ((version.path (update-nemo-build)))

      ;; check if the build lock file exists, meaning a build is currently
      ;; in progress
      (if (file-exists? (build-lock-path (car version.path)))

          `(html (body (p "Build in progress, try again later.")))

          ;; otherwise return the .tar.gz file
          (lambda ()
            (let ((rel-path (release-file-path (car version.path))))
              (parameterize ((root-path (pathname-directory rel-path)))
                (send-file (pathname-strip-directory rel-path))))))))
  no-template: #t)

    

(define-page "/reload"
  (lambda ()
    (reload-apps (awful-apps))
    "Reloaded"))

