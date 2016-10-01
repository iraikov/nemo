;;       
;; 
;; Procedures for construction of ionic current equations from NEMO models. 
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

(module nemo-currents

 (
  nemo:ionic-current-definitions
  )

 (import scheme chicken srfi-1 srfi-13 srfi-69)
 
 (require-extension matchable nemo-core nemo-utils)
 
 (define (cid x)  (second x))
 (define (cn x)   (first x))


 (define (nemo:ionic-current-definitions 
          gate-complexes i-gates i-syns pscs marea
          state-power component-exports component-subcomps
          quantity-name rhsexpr canonicalize-expr
          builtin-fns)

  (let* (
         (i-eqs (filter-map
                 (lambda (gate-complex) 
                   
                   (let* ((label             (first gate-complex))
                          (n                 (second gate-complex))
                          (subcomps          (component-subcomps n))
                          (acc               (lookup-def 'accumulating-substance subcomps))
                          (perm              (lookup-def 'permeating-ion subcomps))
                          (permqs            (and perm (component-exports (cid perm))))
                          (pore              (lookup-def 'pore subcomps))
                          (permeability      (lookup-def 'permeability subcomps))
                          (gates             (filter (lambda (x) (equal? (car x) 'gate)) subcomps))
                          (sts               (map (lambda (gate) (component-exports (cid gate))) gates))
                          )


                     (if (and pore (null? permqs))
                         (nemo:error 'nemo:currents ": ion channel definition " label
                                     "permeating-ion component lacks exported quantities"))

                     (for-each 
                      (lambda (st)
                        (if (null? st)
                            (nemo:error 'nemo:currents ": ion channel definition " label
                                        "gate component lacks exported quantities")))
                      sts)
			 
                     (if (not (or pore permeability))
                         (nemo:error 'nemo:currents ": ion channel definition " label
                                     "lacks any pore or permeability components"))

                     (if permeability
                         (let ((exports (component-exports (cid permeability))))
                           (if (not (>= (length exports) 2))
                               (nemo:error 'nemo:currents ": permeability component " (cn permeability)
                                           "needs to export at least permeability state and maximum permeability"))
                           ))


                     (cond ((and perm permeability (pair? gates))
                            (let* ((i     (quantity-name (s+ 'i (cn perm))))
                                   (pmax  (car (component-exports (cid permeability))))
                                   (pwrs  (map (lambda (st) (map (lambda (n) (state-power n)) st)) sts))
                                   (gpwrs (map (lambda (st pwr) (map (lambda (s p) (if p `(pow ,s ,p) s)) st pwr)) sts pwrs))
                                   (gion  `(* ,pmax ,(sum (map (lambda (gpwr) 
                                                                 (match gpwr ((x)  x) (else `(* ,@gpwr))))
                                                               gpwrs))))
                                   )
                              (list i #f gion (quantity-name (s+ 'i_ label) ))))
                           
                           ((and perm pore (pair? gates))
                            (case (cn perm)
                              ((non-specific)
                               (let* ((i     (quantity-name 'i))
                                      (e     (car permqs))
                                      (gmax  (car (component-exports (cid pore))))
                                      (pwrs  (map (lambda (st) (map (lambda (n) (state-power n)) st)) sts))
                                      (gpwrs (map (lambda (st pwr) (map (lambda (s p) (if p `(pow ,s ,p) s)) st pwr)) sts pwrs))
                                      (gion  `(* ,gmax ,(sum (map (lambda (gpwr) 
                                                                    (match gpwr ((x)  x) (else `(* ,@gpwr))))
                                                                  gpwrs))))
                                      )
                                 (list i e gion  (quantity-name (s+ 'i_ label) ))))
                              
                              (else
                               (let* ((i     (quantity-name (s+ 'i (cn perm))))
                                      (e     (car permqs))
                                      (gmax  (car (component-exports (cid pore))))
                                      (pwrs  (map (lambda (st) (map (lambda (n) (state-power n)) st)) sts))
                                      (gpwrs (map (lambda (st pwr) (map (lambda (s p) (if p `(pow ,s ,p) s)) st pwr)) sts pwrs))
                                      (gion  `(* ,gmax ,(sum (map (lambda (gpwr) 
                                                                    (match gpwr ((x)  x) (else `(* ,@gpwr))))
                                                                  gpwrs))))
                                      )
                                 (list i e gion (quantity-name (s+ 'i_ label) ))))))
                           
                           ((and perm pore)
				(case (cn perm)
				  ((non-specific)
				   (let* ((i     (quantity-name 'i))
					  (e     (car permqs))
					  (gmax  (car (component-exports (cid pore))))
                                          )
				     (list i e gmax (quantity-name (s+ 'i_ label) ))))
				  (else
				   (nemo:error 'nemo:ionic-currents ": invalid ion channel definition " label))))
			       
			       ((and acc pore (pair? gates))
				(let* ((i     (quantity-name (s+ 'i (cn acc))))
				       (gmax  (car (component-exports (cid pore))))
				       (pwrs  (map (lambda (st) (map (lambda (n) (state-power n)) st)) sts))
				       (gpwrs (map (lambda (st pwr) (map (lambda (s p) (if p `(pow ,s ,p) s)) st pwr)) sts pwrs))
				       (gion  `(* ,gmax ,(sum (map (lambda (gpwr) 
								     (match gpwr ((x)  x) (else `(* ,@gpwr))))
								   gpwrs))))
				       )
				  (list i #f gion  (quantity-name (s+ 'i_ label) ))))
			       (else (nemo:error 'nemo:ionic-currents ": invalid ion channel definition " label))
			       )))
		     gate-complexes))

	     (i-names (delete-duplicates (map first i-eqs)))
		
	     (i-eqs  (fold  (lambda (i-gate ax) 
			      (let ((i-gate-var (first i-gate)))
				(cons (list (quantity-name 'i) #f i-gate-var (s+ 'i_ (second i-gate))) ax)))
			    i-eqs i-gates))

	     (i-eqs  (if (and i-syns pscs)
                         (fold  (lambda (isyn psc ax) 
                                  (if marea
                                      (cons (list (first isyn) (third isyn) 
                                                  (second isyn) (s+ 'i_ (first psc)) 
                                                  `(/ 100.0 ,marea))
                                            ax)
                                      (cons (list (first isyn) (third isyn) 
                                                  (second isyn) (s+ 'i_ (first psc)))
                                            ax)))
                                i-eqs i-syns pscs)
                         i-eqs))

	     (i-bkts (bucket-partition (lambda (x y) (eq? (car x) (car y))) i-eqs))
	     
	     (i-eqs  (fold (lambda (b ax) 
			     (match b 
				    ((and ps ((i e gion ii) . rst))  
				     (let loop ((ps ps) (summands (list)) (eqs (list)))
				       (if (null? ps)
					   
					   (let* ((sum0  (sum summands))
						  (sum1  (rhsexpr sum0))
						  (sum2  (add-params-to-fncall 
							  (canonicalize-expr sum1) builtin-fns)))
					     (append eqs (list (list i sum2)) ax))
					   
					   (match (car ps)
                                                  ((i e gion ii)
                                                   (loop (cdr ps) 
                                                         (cons ii summands) 
                                                         (let* ((expr0 (rhsexpr (if e `(* ,gion (- v ,e)) gion)))
                                                                (expr1 (canonicalize-expr expr0)))
                                                           (cons (list ii expr1) eqs))))

                                                  ((i e gion ii k)
                                                   (loop (cdr ps) 
                                                         (cons ii summands) 
                                                         (let* ((expr0 (rhsexpr (if e `(* ,k ,gion (- v ,e)) `(* ,k ,gion))))
                                                                (expr1 (canonicalize-expr expr0)))
                                                           (cons (list ii expr1) eqs))))

                                                  )

                                           ))
                                     )
				    
				    ((i e gion ii k)
				     (let* ((expr0  (rhsexpr (if e `(* ,k ,gion (- v ,e)) `(* ,k ,gion))))
					    (expr1  (canonicalize-expr expr0)))
				       (cons (list i expr1) ax)))

				    ((i e gion ii)
				     (let* ((expr0  (rhsexpr (if e `(* ,gion (- v ,e)) gion)))
					    (expr1  (canonicalize-expr expr0)))
				       (cons (list i expr1) ax)))
				    
				    (else ax)))

			   (list) i-bkts))

             )

    `((i-eqs ,i-eqs)
      (i-names ,i-names))

    ))


	  
)
