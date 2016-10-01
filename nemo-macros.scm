;;
;; NEMO macros
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

(module nemo-macros
	
	(nemo-begin nemo-model nemo-transform)

	(import scheme chicken srfi-1 srfi-69)
	
	(require-extension matchable  nemo-core )
	(import-for-syntax matchable  nemo-core)

(define-syntax nemo-begin
  (lambda (f r c)
    (let ((sys  (cadr f))
	  (body (cddr f))
	  (%begin  (r 'begin))
	  (%if     (r 'if))
	  (%let    (r 'let))
	  (%match  (r 'match)))
      `(,%begin
	 (,%if (not (hash-table? ,sys)) (nemo:error 'nemo-begin "system argument must be an environment"))
	 (,%let ((nemo (,%match (hash-table-ref ,sys (nemo-intern 'nemocore))
				(($ nemo:quantity 'DISPATCH value)  value))))
		,@body)))))


(define-syntax nemo-model 
  (lambda (f r c)
    (let ((name  (cadr f))
	  (declarations (caddr f))
	  (body         (cdddr f))
	  (%begin    (r 'begin))
	  (%let*     (r 'let*)))
      `(,%begin
	(,%let* ((nemo   (make-nemo-core))
		 (,name     ((nemo 'system) ',name)))
		(eval-nemo-system-decls nemo ',name ,name (list ,@(map (lambda (x) (list 'quasiquote x)) declarations)))
		,@body)))))
		   

(define-syntax nemo-transform
  (lambda (f r c)
    (let ((sys  (cadr f))
	  (declarations (caddr f))
	  (body         (cdddr f))
	  (%begin    (r 'begin))
	  (%if       (r 'if))
	  (%match    (r 'match))
	  (%let*     (r 'let*)))
      `(,%begin
	(,%if (not (hash-table? ,sys)) (nemo:error 'nemo-transform "system argument must be an environment"))
	(,%let* ((nemo  (,%match (hash-table-ref ,sys (nemo-intern 'dispatch))
				 (($ nemo:quantity 'DISPATCH value)  value)))
		 (sys1 (nemo:env-copy ,sys))
		 (name ((nemo 'sysname) sys1)))
		(eval-nemo-system-decls nemo name sys1 (list ,@(map (lambda (x) (list 'quasiquote x)) declarations)))
		sys1)))))

)
