;;
;; 
;; An extension for specifying Hodgkin-Huxley type dynamics in NEMO
;; systems.
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

(module nemo-hh

	(nemo:hh-transformer)

	(import scheme chicken data-structures srfi-1 srfi-13 srfi-69)
	
	(require-extension matchable varsubst nemo-core)

(define (s+ . lst)    (string-concatenate (map ->string lst)))

(define (p$ p n) (gensym (string->symbol (s+ (->string p) "_" (->string n)))))


(define (lookup-field k lst . rest)
  (let-optionals rest ((default #f))
   (let ((v (alist-ref k lst)))
     (if v (first v) default))))


(define (check-names ion env . names)
  (for-each (lambda (name)
	      (if (hash-table-exists? env name)
		  (nemo:error 'nemo:hh-transformer "quantity " name " in ionic conductance declaration " ion
			     "is already declared elsewhere")))
	    names))

(define (check-decls ion names alst . rest)
  (let-optionals rest ((raise-exception? #t))
    (if raise-exception?
	(for-each (lambda (name) 
		    (if (not (alist-ref name alst))
			(nemo:error 'nemo:hh-transformer "required quantity" name 
				   " is not present in ionic conductance declaration" ion)))
		  names)
	(every (lambda (name) (alist-ref name alst)) names))))


(define (handle thunk dflt)
  (condition-case (thunk)
		  [(exn) dflt]))

(define (hh-ionic-gate-transform sys parse-expr subst-expr scope-subst scope 
                                 eval-const env-extend! add-external! component-extend! 
                                 comp en markov? )
  (define (and-parse-expr scope-subst x . rest) 
    (and x (subst-expr (apply parse-expr (cons x rest)) scope-subst)))
  (match en
	 ((or (('hh 'ionic 'conductance)  ('name (? symbol? ion)) . alst)
	      (('hh-ionic-gate)    ('name (? symbol? ion)) . alst))

	  (check-decls ion '(m-power h-power) alst)

	  (let* ((suffix (->string ion))
		 
		 (m-inf-sym   (p$ ion 'm-inf))
		 (m-tau-sym   (p$ ion 'm-tau))
		 
		 (h-inf-sym   (p$ ion 'h-inf))
		 (h-tau-sym   (p$ ion 'h-tau))
		 )


	    (let ((m-power (eval-const sys (subst-expr
					    (parse-expr (lookup-field 'm-power alst) 
							`(hh-ionic-gate ,ion (m-power))
							)
					    scope-subst)
                                       (sprintf "~A.m-power" ion)))
		  (h-power (eval-const sys (subst-expr
					    (parse-expr (lookup-field 'h-power alst 0)
							`(hh-ionic-gate ,ion (h-power))
							)
                                            scope-subst)
                                       (sprintf "~A.h-power" ion)
                                       ))
                  )


	    (if (not (and (integer? m-power) (positive? m-power)))
		(nemo:error 'nemo:hh-transformer 
			   "m-power value in ionic conductance declaration " ion
			   " must be a positive integer"))
	    
	    ;; check for required decls in m
	    (check-decls ion '(initial-m) alst)
	    (if (not (check-decls ion '(m-inf m-tau) alst #f))
		(check-decls ion '(m-alpha m-beta) alst))
	    
	    ;; check for required decls in h
	    (if (positive? h-power) 
		(begin (check-decls ion '(initial-h) alst)
		       (if (not (check-decls ion '(h-inf h-tau) alst #f))
			   (check-decls ion '(h-alpha h-beta) alst))))
	    
	    (if (not (and (integer? h-power) (or (zero? h-power) (positive? m-power))))
		(nemo:error 'nemo:hh-transformer 
			   "h-power value in ionic conductance declaration " ion
							" must be a positive integer"))

	    (let* ((initial-m  ((lambda (x) 
				  (let ((expr (subst-expr
					       (parse-expr x `(hh-ionic-gate ,ion (initial-m))) 
					       scope-subst)))
				    (handle (lambda () (eval-const sys expr (sprintf "~A.initial-m" ion))) expr)))
				(lookup-field 'initial-m alst)))
		   (m-inf      (and-parse-expr scope-subst
					       (lookup-field 'm-inf alst)
					       `(hh-ionic-gate ,ion (m-inf))))
		   (m-tau      (and-parse-expr scope-subst
					       (lookup-field 'm-tau alst)
					       `(hh-ionic-gate ,ion (m-tau))))
		   (m-alpha    (and-parse-expr scope-subst
					       (lookup-field 'm-alpha alst)
					       `(hh-ionic-gate ,ion (m-alpha))))
		   (m-beta     (and-parse-expr scope-subst
					       (lookup-field 'm-beta alst)
					       `(hh-ionic-gate ,ion (m-beta))))
		   (open       'O)
		   (closed     'C)
		   )

	      (if (and m-inf (not (equal? m-inf m-inf-sym)))
                  (begin
                    (env-extend! m-inf-sym '(asgn) 'none `(rhs ,m-inf))
                    (component-extend! comp m-inf-sym)))

	      (if (and m-tau (not (equal? m-inf m-inf-sym)))
                  (begin
                    (env-extend! m-tau-sym '(asgn) 'none `(rhs ,m-tau))
                    (component-extend! comp m-tau-sym)))

	      (cond ((or (and m-alpha m-beta) (and markov? m-tau m-inf))
		     (let* ((m-reaction-sym  (p$ ion 'm))
			    (m-alpha    (or m-alpha
					    (subst-expr 
					     `(let ((x (/ ,m-inf-sym ,m-tau-sym))) x)
					     scope-subst)))
			    (m-beta     (or m-beta
					    (subst-expr 
					     `(let ((x (/ (- 1 ,m-inf-sym) ,m-tau-sym))) x)
					     scope-subst)))
			    (mst        `((power ,m-power)  (open  ,open)
					  (transitions (<-> ,closed ,open ,m-alpha ,m-beta))
					  (conserve (1 = (+ ,closed ,open))))))
		       (apply env-extend! (cons* m-reaction-sym '(reaction) initial-m mst))
		       (add-external! m-reaction-sym 'output)
		       (component-extend! comp m-reaction-sym)))
		    ((and m-tau m-inf)
		     (let* ((m-rate-sym  (p$ ion 'm))
			    (rate-rhs    `((power ,m-power)
					   (rhs  (/ (- ,m-inf-sym ,m-rate-sym) ,m-tau-sym))
					   )))
		       (apply env-extend! (cons* m-rate-sym '(rate) initial-m rate-rhs))
		       (add-external! m-rate-sym 'output)
		       (component-extend! comp m-rate-sym)
		       ))
		    (else
		     (nemo:error 'nemo:hh-transformer 
				 "invalid activation and inactivation rate specification in ionic conductance declaration " 
				 ion)))
	      )
	    
	    (if (positive? h-power)
		(let* ((initial-h  ((lambda (x)
				      (let ((expr (subst-expr 
						   (parse-expr x `(hh-ionic-gate ,ion (initial-h)))
						   scope-subst)))
					(handle (lambda () (eval-const sys expr (sprintf "~A.initial-h" ion))) expr)))
				    (lookup-field 'initial-h alst)))
		       (h-inf      (and-parse-expr scope-subst
						   (lookup-field 'h-inf alst)
						   `(hh-ionic-gate ,ion (h-inf))))
		       (h-tau      (and-parse-expr scope-subst
						   (lookup-field 'h-tau alst)
						   `(hh-ionic-gate ,ion (h-tau))))
		       (h-alpha    (and-parse-expr scope-subst
						   (lookup-field 'h-alpha alst)
						   `(hh-ionic-gate ,ion (h-alpha))))
		       (h-beta     (and-parse-expr scope-subst
						   (lookup-field 'h-beta alst)
						   `(hh-ionic-gate ,ion (h-beta))))
		       (open       'O)
		       (closed     'C))

		  (if (and h-inf (not (equal? h-inf h-inf-sym)))
                      (begin
                        (env-extend! h-inf-sym '(asgn) 'none `(rhs ,h-inf))
                        (component-extend! comp h-inf-sym)))

		  (if (and h-tau (not (equal? h-tau h-tau-sym)))
                      (begin
                        (env-extend! h-tau-sym '(asgn) 'none `(rhs ,h-tau))
                        (component-extend! comp h-tau-sym)))

		  (cond ((or (and h-alpha h-beta) (and markov? h-tau h-inf))
			 (let* ((h-reaction-sym  (p$ ion 'h))
				(h-alpha    (or h-alpha
						(subst-expr 
						 `(let ((x (/ ,h-inf-sym ,h-tau-sym))) x)
						 scope-subst)))
				(h-beta     (or h-beta
						(subst-expr
						 `(let ((x (/ (- 1 ,h-inf-sym) ,h-tau-sym))) x)
						 scope-subst)))
				(hst        `((power ,h-power)  (open  ,open)
					      (transitions (<-> ,closed ,open ,h-alpha ,h-beta))
					      (conserve (1 = (+ ,closed ,open))))))
			   (apply env-extend! (cons* h-reaction-sym '(reaction) initial-h hst))
			   (add-external! h-reaction-sym 'output)
			   (component-extend! comp h-reaction-sym)))
			((and h-tau h-inf)
			 (let* ((h-rate-sym  (p$ ion 'h))
				(rate-rhs    `((power ,h-power)
					       (rhs (/ (- ,h-inf-sym ,h-rate-sym) ,h-tau-sym)))))
			   (apply env-extend! (cons* h-rate-sym '(rate) initial-h rate-rhs))
			   (add-external! h-rate-sym 'output)
			   (component-extend! comp h-rate-sym)
			   ))
			(else
			 (nemo:error 'nemo:hh-transformer 
				     "invalid activation and inactivation rate specification in ionic conductance declaration " 
				     ion)))

		  )))))

	 (else (list))))

(define (nemo:hh-transformer sys markov? . rest)
  (let-optionals rest ((parse-expr (lambda (x . rest) (identity x))))
   (let ((new-sys  (nemo:env-copy sys)))
     (match-let ((($ nemo:quantity 'DISPATCH  dis) (hash-table-ref new-sys (nemo-intern 'dispatch))))
      (let* ((eval-const         (dis 'eval-const))
	     (subst-expr         (dis 'subst-expr))
	     (env-extend!        ((dis 'env-extend!) new-sys))
	     (add-external!      ((dis 'add-external!) new-sys))
	     (component-extend!  ((dis 'component-extend!) new-sys))
	     (indent  0)
	     (indent+ (+ 2 indent )))
	(let recur ((comp-name (nemo-intern 'toplevel)) (scope #f))
	  (let* ((comp-symbols   ((dis 'component-symbols) new-sys comp-name))
		 (subcomps       ((dis 'component-subcomps) new-sys comp-name))
		 (scope-subst    ((dis 'component-scope-subst) new-sys comp-name)))
	    (for-each (lambda (sym)
			(hh-ionic-gate-transform  
			 new-sys parse-expr subst-expr scope-subst scope
			 eval-const env-extend! add-external! component-extend!
			 comp-name (hash-table-ref new-sys sym) markov?))
		      comp-symbols)
	    (for-each (lambda (subcomp) (recur subcomp (or scope subcomp))) (map third subcomps))))
	new-sys)))))
)
