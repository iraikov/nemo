;;       
;; Fetch NEMO/NineML components from file and http URLs.
;;
;; Copyright 2008-2016 Ivan Raikov.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; A full copy of the GPL license can be found at
;; <http://www.gnu.org/licenses/>.
;;

(module nemo-fetch

	(nemo:fetch nemo:fetch-verbose)

	(import scheme chicken extras utils data-structures files posix srfi-1 srfi-13 tcp irregex)
        (require-extension matchable uri-generic)

        (define nemo:fetch-verbose (make-parameter #f))
        
        (define (d fstr . args)
          (let ([port (current-error-port)])
            (if (nemo:fetch-verbose)
                (begin (apply fprintf port fstr args)
                       (flush-output port) ) )))

        (define (string-match rx str)
          (and-let* ((m (irregex-match rx str)))
            (let loop ((i (irregex-match-num-submatches m))
                       (res '()))
              (if (fx<= i 0)
                  (cons str res)
                  (loop (fx- i 1) (cons (irregex-match-substring m i) res))))))


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
        
        
        (define (http-fetch uri dest)
          (d "fetching ~s ...~%" (uri->string uri))
          (match-let (((_ ((_ host port) ('/ . path) query) _) (uri->list uri)))
                     (let* ((port      (or port 80))
                            (locn      (uri->string (update-uri (update-uri uri scheme: #f) host: #f)))
                            (query     (and query (not (string-null? query)) query))
                            (filedir   (uri-decode-string (string-concatenate (intersperse (if query path (drop-right path 1)) "/"))))
                            (filename  (uri-decode-string (or (and query (cadr (string-split query "="))) (last path))))
                            (dest      (make-pathname dest filedir))
                            (filepath  (make-pathname dest filename)))
                       (if (file-exists? filepath) filepath
                           (begin
                             (d "connecting to host ~s, port ~a ...~%" host port)
                             (let-values ([(in out) (tcp-connect host port)])
                               (d "requesting ~s ...~%" locn)
                               (display
                                (make-HTTP-GET/1.1 locn "NineML" host port: port accept: "*/*")
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
                                                  (if (not (file-exists? dest)) (create-directory dest))
                                                  (d "writing to ~s~%" filepath)
                                                  (with-output-to-file filepath (cut display data) )
                                                  filepath))
                                              
                                              (begin
                                                (d "reading data ...~%")
                                                (let ([data (read-string #f in)])
                                                  (close-input-port in)
                                                  (close-output-port out)
                                                  (if (not (file-exists? dest)) (create-directory dest))
                                                  (d "writing to ~s~%" filepath)
                                                  (with-output-to-file filepath (cut display data) binary:)
                                                  filepath)))))
                                   )
                                 )))))))

        (define (read-chunks in)
          (let get-chunks ([data '()])
            (let ([size (string->number (read-line in) 16)])
              (if (zero? size)
                  (string-concatenate-reverse data)
                  (let ([chunk (read-string size in)])
                    (read-line in)
                    (get-chunks (cons chunk data)) ) ) ) ) )


        (define (nemo:fetch uri)

          (case (uri-scheme uri)

            ((http)
             (let ((tmpdir (or (get-environment-variable "TMPDIR") "/tmp")))
               (let-values (((fd temp-path) (file-mkstemp (make-pathname tmpdir "nemo.XXXXXX"))))
                 (let ((data (and (http-fetch uri temp-path) (read-all temp-path))))
                   (file-close fd)
                   data))))

            ((file)
             (let ((data (read-all 
                          (string-concatenate
                           (intersperse (map ->string (uri-path uri)) "/")))))
               data))
            
            (else (error 'fetch "unknown scheme" (uri-scheme uri)))

            ))
)

