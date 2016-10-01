
;;
;; Continuous integration scripts for model development.
;;

(use matchable data-structures posix files tcp srfi-1 srfi-13 regex setup-api uri-generic awful sendfile)
(require-library spiffy intarweb)
(import (only spiffy current-request current-response write-logged-response 
	      with-headers mime-type-map root-path file-extension->mime-type send-static-file)
	(only intarweb response-port request-method  request-headers
	      header-values header-value etag-matches? response-has-message-body-for-request?
	      ))

(enable-sxml #t)


(define v:quiet 0)
(define v:info  1)
(define v:debug 2)

(define http-user-agent "nemo-model")

(define verbose (make-parameter v:info))

(define prefix (make-parameter (get-environment-variable "HOME")))

(define models (make-parameter '()))

(define config-path
 (let ((path (get-environment-variable "MODEL_CI_CONFIG")))
   (or path "model-ci.config")))

(load config-path)


(define (version-path)
  (make-pathname (prefix) "/build/model-ci.versions"))
  
(define (build-location-prefix)
  (make-pathname (prefix) "/build/model-ci"))

(define (build-location model-name version) 
  (make-pathname (build-location-prefix) 
		 (sprintf "~A.~A" model-name version)))

(define (build-log-path model-name version) 
  (make-pathname (build-location model-name version) 
		 (string-append (sprintf "~A-log." model-name) 
				(->string version)) ))

(define (build-lock-path model-name version) 
  (make-pathname (build-location model-name version) 
		 (string-append (sprintf "~A-build-lock." model-name)
				(->string version)) ))

(define (tests-lock-path model-name version) 
  (make-pathname (build-location model-name version) 
		 (string-append (sprintf "~A-tests-lock." model-name)
				(->string version)) ))

(define (tests-log-path model-name version) 
  (make-pathname (build-location model-name version) 
		 (string-append (sprintf "~A-tests-log." model-name)
				(->string version)) ))

(define (plots-lock-path model-name version) 
  (make-pathname (build-location model-name version) 
		 (string-append (sprintf "~A-plots-lock." model-name)
				(->string version)) ))

(define (plots-log-path model-name version) 
  (make-pathname (build-location model-name version) 
		 (string-append (sprintf "~A-plots-log." model-name)
				(->string version)) ))

(debug-file (make-pathname (build-location-prefix) "/debug.log"))
;;(error-log (make-pathname build-location-prefix "/debug.log"))




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
     (call-with-input-pipe (sprintf "~a" cmd) lam))
   (smooth cmd)))


(define-syntax ipipe
  (syntax-rules ()
    ((_ lam exp)
     (ipipe:execute lam `exp ))))


;; From spiffy
(define (call-with-input-file* file proc)
  (call-with-input-file
      file (lambda (p)
	     (handle-exceptions exn
		(begin (close-input-port p) (raise exn))
		(proc p)))))

(define (call-with-output-file* file proc)
  (call-with-output-file
      file (lambda (p)
	     (handle-exceptions exn
		(begin (close-output-port p) (raise exn))
		(proc p)))))


(define (revisions-command model-name config)
  (or (alist-ref 'revision-command config)
      (alist-ref 'revisions-command config)
      (let ((config-dir (alist-ref 'config-path config)))
	(if (not config-dir)
	    (error 'revisions-command "unable to find model revisions command"))
	(make-pathname config-dir "revisions"))))


(define (fetch-command model-name config)
  (or (alist-ref 'fetch-command config)
      (alist-ref 'fetch-command config)
      (let ((config-dir (alist-ref 'config-path config)))
	(if (not config-dir)
	    (error 'fetch-command "unable to find model fetch command"))
	(make-pathname config-dir "fetch"))))


(define (build-command model-name config)
  (or (alist-ref 'build-command config)
      (alist-ref 'build-command config)
      (let ((config-dir (alist-ref 'config-path config)))
	(if (not config-dir)
	    (error 'build-command "unable to find model build command"))
	(make-pathname config-dir "build"))))


(define (test-commands model-name config)
  (or (alist-ref 'test-commands config)
      (alist-ref 'test-command config)
      (let ((config-dir (alist-ref 'config-path config)))
	(if (not config-dir)
	    (error 'test-command "unable to find model test commands"))
	(let ((tests-run-path (make-pathname config-dir "tests/run")))
	  (if (file-exists? tests-run-path)
	       (list tests-run-path)
	       (let ((flst (find-files (make-pathname config-dir "tests") 
				       limit: 1
				       test: file-execute-access?)))
		 (sort flst string<?)
		 ))
	      ))
	))


(define (plot-commands model-name config)
  (or (alist-ref 'plot-commands config)
      (alist-ref 'plot-command config)
      (let ((config-dir (alist-ref 'config-path config)))
	(if (not config-dir)
	    (error 'plot-command "unable to find model plot commands"))
	(list (make-pathname config-dir "plots")))))


(define (build model-name build-dir local-version version lock-file log-file fetch-cmd build-cmd )
  (if (not (file-exists? lock-file))
      (call-with-output-file* log-file
	(lambda (out)
	  (run (mkdir -p ,build-dir))
	  (run (touch ,lock-file))
	  (if (or (not local-version) (not (string=? version local-version)))
	      (begin
		(run- (,fetch-cmd ,model-name ,version ,build-dir >> ,log-file 2>&1  )
		      (,build-cmd ,model-name ,build-dir >> ,log-file 2>&1 ))
		(let ((versions (call-with-input-file* (version-path) read)))
		  (let ((versions1 (if (pair? versions) 
				       (alist-update model-name version versions)
				       (list (cons model-name version)))))
		    (call-with-output-file* (version-path)
					    (lambda (out) (write versions1 out ))))
		  )))
	  (run (rm ,lock-file))
	  ))
      ))


(define (run-tests model-name build-dir version lock-file log-file cmds)
  (if (not (file-exists? lock-file))
      (call-with-output-file* log-file
        (lambda (out)				 
	  (run (touch ,lock-file))
	  (for-each (lambda (cmd) (run- (,cmd ,model-name ,build-dir >> ,log-file  2>&1  ))) cmds)
	  (run (rm ,lock-file))
	  ))
      ))


(define (make-plots model-name build-dir version lock-file log-file cmds)
  (if (not (file-exists? lock-file))
      (call-with-output-file* log-file
	(lambda (out)
	  (run (touch ,lock-file))
	  (for-each (lambda (cmd) (run- (,cmd ,model-name ,build-dir >> ,log-file  2>&1  ))) cmds)
	  (run (rm ,lock-file))
	  ))
      ))


(define (update-model model-name config)

  (if (not (file-exists? (version-path)))
      (let* ((path (version-path))
	     (dir (pathname-directory path)))
	(run- (mkdir -p ,dir) (touch ,path))))

  (let ((versions (call-with-input-file* (version-path) read)))

    (let ((local-version (and versions (pair? versions) (alist-ref model-name versions)))
	  (remote-version (string-trim-both  (car (ipipe (lambda (x) (read-lines x)) (,(revisions-command model-name config) ,model-name))))))

      (let ((loc (build-location model-name remote-version)))
	(if (not (file-exists? loc))
	    (let ((build-lock-file (build-lock-path model-name remote-version))
		  (build-log-file (build-log-path model-name remote-version))
		  (test-lock-file (tests-lock-path model-name remote-version))
		  (test-log-file  (tests-log-path model-name remote-version))
		  (plot-lock-file (plots-lock-path model-name remote-version))
		  (plot-log-file  (plots-log-path model-name remote-version))
		  )
	      (process-fork
	       (lambda ()
		 (run (mkdir -p ,loc))
		 (run (touch ,build-log-file  ,test-log-file ,plot-log-file))
		 (build model-name loc local-version remote-version 
			build-lock-file build-log-file
			(fetch-command model-name config)
			(build-command model-name config))
		 (run-tests model-name loc remote-version 
			    test-lock-file test-log-file
			    (test-commands model-name config))
		 (make-plots model-name loc remote-version 
			     plot-lock-file plot-log-file
			     (plot-commands model-name config))
		 (exit 0)
		 ))
	      ))
	(list remote-version loc))
      )))

(define-page "/models"
  (lambda ()
    `((h1 "NEMO model status")
      ,(map 
	(lambda (kv)
	  (let* ((model-name (car kv))
		 (model-config (cdr kv))
		 (model-label (alist-ref 'label model-config))
		 )
	    
	    `(p ,(link (sprintf "/model-status?name=~A" model-name)
		       (or model-label (sprintf "Model ~A" model-name))))
	    
	    ))
	(models)))
    ))
   

(define-page "/model-status"
  (lambda ()
    (let* ((model-name (string->symbol ($ 'name)))
	   (model-config (alist-ref model-name (models)))
	   (model-label (alist-ref 'label model-config))
	   )
      
      (if (not model-config)
	  `(p ,(sprintf "Invalid model name ~A" model-name))
	  (let ((version.path (update-model model-name model-config)))
	    (cond ((file-exists? (build-lock-path model-name (car version.path)))
		   `(p "Build in progress, try again later."))
		  ((file-exists? (tests-lock-path model-name (car version.path)))
		   `(p "Tests in progress, try again later."))
		  ((file-exists? (plots-lock-path model-name (car version.path)))
		   `(p "Plots in progress, try again later."))
		  (else
		   `((h1 ,(or model-label (sprintf "Model ~A" model-name )))
		     (p)
		     (p ,(sprintf "The current version of ~A is ~A.~%" model-name (car version.path)))
		     ,(model-png-plots model-name (car version.path) (cadr version.path))
		     (p ,(link (sprintf "/model-build-log?name=~A" model-name)
			       (sprintf "Model build log version ~A~%" 
					(car version.path))))
		     (p ,(link (sprintf "/model-test-log?name=~A" model-name)
			       (sprintf "Model test log version ~A~%" 
					(car version.path))))
		     (p ,(link (sprintf "/model-plot-log?name=~A" model-name)
			       (sprintf "Model plot log version ~A~%" 
					(car version.path))))
		     ))
		  ))
	  ))
    ))
	

   
(define (model-png-plots model-name model-version model-loc)
  (let ((jpgpat  "(.*\\.[jJ][pP][eE]?[gG]$)")
	(pngpat  "(.*\\.[pP][nN][gG]$)"))
    (let ((pat   (string-append jpgpat "|" pngpat)))
      (let ((flst (find-files model-loc test: (regexp pat))))
	(map (lambda (f) 
	       (let ((fn (pathname-strip-directory f)))
		 `(p (img (@ (src ,(sprintf "/model-plot?modelname=~A&plotname=~A" model-name fn))) ))
		 )) flst)))
    ))

(define-page "/model-plot"      
  (lambda () 
    (let* (
	   (model-name (string->symbol ($ 'modelname)))
	   (model-config (alist-ref model-name (models)))
	   (plot-name  ($ 'plotname))
	   )
      (if (not model-config)
	  `(html (body (p ,(sprintf "Invalid model name ~A" model-name))))
	  (let ((version.path (update-model model-name model-config)))
	    (lambda ()
	      (let ((plot-file-path (cadr version.path)))
		(parameterize ((root-path plot-file-path))
			      (send-static-file plot-name))
		))
	    ))
      ))
  no-template: #f)
   
(define-page "/model-build-log"
  (lambda () 
    (let* ((model-name (string->symbol ($ 'name)))
	   (model-config (alist-ref model-name (models)))
	   )
      (if (not model-config)
	  `(p ,(sprintf "Invalid model name ~A" model-name))
	  (let ((version.path (update-model  model-name model-config)))
	    `((p "Build log:")
	      (pre . ,(intersperse (read-lines (build-log-path model-name (car version.path))) "\n")))
	    ))
      )))

   
(define-page "/model-test-log"
  (lambda () 
    (let* ((model-name (string->symbol ($ 'name)))
	   (model-config (alist-ref model-name (models)))
	   )
      (if (not model-config)
	  `(p ,(sprintf "Invalid model name ~A" model-name))
	  (let ((version.path (update-model  model-name model-config)))
	    `((p "Test log:")
	      (pre . ,(intersperse (read-lines (tests-log-path model-name (car version.path))) "\n")))
	    ))
      )))

   
(define-page "/model-plot-log"
  (lambda () 
    (let* ((model-name (string->symbol ($ 'name)))
	   (model-config (alist-ref model-name (models)))
	   )
      (if (not model-config)
	  `(p ,(sprintf "Invalid model name ~A" model-name))
	  (let ((version.path (update-model  model-name model-config)))
	    `((p "Plot log:")
	      (pre . ,(intersperse (read-lines (plots-log-path model-name (car version.path))) "\n")))
	    ))
      )))


(define-page "/reload"
  (lambda ()
    (reload-apps (awful-apps))
    (load config-path)
    "Reloaded"))
