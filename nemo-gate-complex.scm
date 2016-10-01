;;       
;; 
;; Procedures for querying ion channel descriptions in NEMO models. 
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

(module nemo-gate-complex

 (nemo:ion-pool-query
  nemo:gate-complex-query
  nemo:resolve-imports

  pool-ion? 
  pool-ion-name-map
  pool-ion-name
  pool-ion-inq
  pool-ion-outq
  pool-ion-cur
  pool-ion-in
  pool-ion-out
  pool-ion-valence
  )

 (import scheme chicken srfi-1 srfi-13 srfi-69)

 (require-extension matchable nemo-core nemo-utils)

(define (cid x)  (second x))
(define (cn x)   (first x))

(define-record-type pool-ion (make-pool-ion ion cur inq outq in out valence)
  pool-ion? 
  (ion       pool-ion-name)
  (inq       pool-ion-inq)
  (outq      pool-ion-outq)
  (cur       pool-ion-cur)
  (in        pool-ion-in)
  (out       pool-ion-out)
  (valence   pool-ion-valence)
  )

(define (pool-ion-name-map f ps)
  (map
   (lambda (p)
     (make-pool-ion 
      (f (pool-ion-name p))
      (f (pool-ion-cur p))
      (f (pool-ion-inq p))
      (f (pool-ion-outq p))
      (f (pool-ion-in  p))
      (f (pool-ion-out p))
      (pool-ion-valence p)))
   ps))
     
		 

(define (ispool? x)
  (match x (('decaying-pool name id) id) 
	 (('decaying 'pool name id) id) 
	 (else #f)))

(define (accumulating-ion x)
  (match x (('accumulating-ion name id) name) 
	 (('accumulating 'ion name id) name) 
	 (else #f)))

(define (nemo:resolve-imports defaults imports exports epools)
  (let*
      ((unresolved-imports-lnames
        (lset-difference eq? 
                         (lset-difference eq? (map first imports) exports)
                         (cons 'v (map first defaults))))

       (pool-ion-concs (concatenate
                        (map (lambda (x) 
                               (list (pool-ion-in x)
                                     (pool-ion-out x)))
                             epools)))

       (pool-ion-curs (concatenate
                       (map (lambda (x) 
                              (list (pool-ion-cur x)))
                            epools)))
       
       (unresolved-imports0
        (map (lambda (x) (assoc x imports)) unresolved-imports-lnames))
       
       (unresolved-imports
        (filter (lambda (x) 
                  (match x
                         ((lname gname 'ion-pools) (not (member gname pool-ion-concs)))
                         ((lname gname 'ion-currents) (not (member gname pool-ion-curs)))
                         (else x)))
                unresolved-imports0))
       )
    unresolved-imports))
       


(define (nemo:ion-pool-query sys)
   (match-let ((($ nemo:quantity 'DISPATCH  dis) (hash-table-ref sys (nemo-intern 'dispatch))))
     (let recur ((comp-name (nemo-intern 'toplevel)) (ax (list)))
       (let ((subcomps  ((dis 'component-subcomps)  sys comp-name)))
	 (let-values (((epool-comps other-comps)  (partition ispool? subcomps)))
		     (let ((epools (map (lambda (x) 
                                          (let* ((comp-name (third x))
                                                 (subcomps ((dis 'component-subcomps) sys comp-name)))
                                            (let ((ion-name (or (let ((v (find accumulating-ion subcomps)))
                                                                  (and v (second v)))
                                                                (second x)))
                                                  (exports ((dis 'component-exports) sys comp-name)))
                                              (if (null? exports)
                                                  (nemo:error 'nemo:epool-query 
                                                              ": ion pool component " (third x)
                                                              " must export a state"))
                                              `(,ion-name ,comp-name . ,exports))))
					   epool-comps)))
		       (fold recur (append epools ax) (map third other-comps))))))))


(define (nemo:gate-complex-query sys . rest)
  (let-optionals rest ((ionic-current-name     
			(lambda (ion-name) (string->symbol (s+ 'i ion-name))))
		       (rev-potential-name     
			(lambda (ion-name) (string->symbol (s+ 'e ion-name ))))
		       (in-concentration-name  
			(lambda (ion-name) (string->symbol (s+ ion-name 'i))))
		       (out-concentration-name 
			(lambda (ion-name) (string->symbol (s+ ion-name 'o)))))

  (match-let ((($ nemo:quantity 'DISPATCH  dis) 
	       (hash-table-ref sys (nemo-intern 'dispatch))))
    (let ((imports  ((dis 'imports)  sys))
	  (exports  ((dis 'exports)  sys)))
      (let* ((consts      ((dis 'consts)  sys))
	     (asgns       ((dis 'asgns)   sys))
	     (states      ((dis 'states)  sys))
	     (reactions   ((dis 'reactions) sys))
	     (rates       ((dis 'rates) sys))
	     (defuns      ((dis 'defuns)  sys))
	     (components  ((dis 'components) sys))
	     (gate-complexes
	      (filter-map (match-lambda 
			   ((name 'gate-complex id) (list name id)) 
			   ((name 'ionic-current id) (list name id)) 
			   (else #f)) 
			  components))
	     (ion-pools   (nemo:ion-pool-query sys))

	     (perm-ions         (fold (lambda (gate-complex ax) 
					(let* ((subcomps ((dis 'component-subcomps) sys (cid gate-complex)))
					       (perm      (lookup-def 'permeating-ion subcomps)))
					  (cond (perm 
						 (let ((ep ((dis 'component-exports) sys (cid perm))))
						   (case (cn perm)
						     ((non-specific)   
						      (let ((erev (car ep)) (i 'i) (e 'e))
							(cons `(,(cn perm) ,i ,e ,erev #f) ax)))
						     (else (let* ((erev ((lambda (x) (and (pair? x) (car x))) ep))
								  (i    (ionic-current-name (cn perm)))
								  (e    (rev-potential-name (cn perm)))
								  (valence ((lambda (x) (and (pair? x) (pair? (cdr x)) (cadr x))) ep)))
							     (cons `(,(cn perm) ,i ,e ,erev ,valence) ax))))))
						(else ax))))
				      (list) gate-complexes))
	     
	     (perm-ions         (fold (lambda (gate-complex ax) 
					(let* ((subcomps ((dis 'component-subcomps) sys (cid gate-complex)))
					       (bingate   (lookup-def 'binary-gate subcomps)))
					  (cond 
						(bingate 
						 (cons `(non-specific i e #f) ax))
						(else ax))))
				      perm-ions gate-complexes))
	     
	     (acc-ions           (fold (lambda (gate-complex ax) 
					 (let* ((subcomps ((dis 'component-subcomps) sys (cid gate-complex)))
						(acc   (lookup-def 'accumulating-substance subcomps))
						(i     (and acc (ionic-current-name (cn acc))))
						(in    (and acc (in-concentration-name (cn acc))))
						(out   (and acc (out-concentration-name (cn acc)))))
					   (if acc  (cons `(,(cn acc) ,i ,in ,out) ax) ax)))
				       (list) gate-complexes))

	     (pool-ions          (map (lambda (ep)
					(if (< (length ep) 4)
					    (nemo:error 'nemo:gate-complex-query 
							": pool definition " ep
							" must export at least internal and external concentration quantities"))
					(let* ((ion     (first ep))
					       (inq     (third ep))
					       (outq    (fourth ep))
					       (valence (and (> (length ep) 4) (fifth ep)))
					       (cur     (ionic-current-name ion))
					       (in      (in-concentration-name ion))
					       (out     (out-concentration-name ion))
					       )
					  (make-pool-ion ion cur inq outq in out valence)))
				      ion-pools))

	     (mod-ions         (fold (lambda (gate-complex ax) 
					(let* ((subcomps ((dis 'component-subcomps) sys (cid gate-complex)))
					       (modcomp   (lookup-def 'modulating-ion subcomps)))
					  (cond 
						(modcomp 
                                                 (let ((ion (cn modcomp))
						       (valence (let ((ep ((dis 'component-exports) sys (cid modcomp))))
								  (and (> (length ep) 0) (first ep))))
						       )
                                                   (cons `(,ion ,(in-concentration-name ion) 
								,(out-concentration-name ion)
								,valence) ax)))
						(else ax))))
				      '() gate-complexes))

	     (i-gates           (fold (lambda (gate-complex ax) 
					(let* ((subcomps     ((dis 'component-subcomps) sys (cid gate-complex)))
					       (i-gate-comp  (lookup-def 'binary-gate subcomps)))
					  (if i-gate-comp
					      (let* ((i-gate-exps  ((dis 'component-exports) sys (cid i-gate-comp)))
						     (asgn-names   asgns)
						     (i-gate-var   (find (lambda (x) (member x asgn-names)) 
									 i-gate-exps)))
						(if (not i-gate-var)
						    (nemo:error 'nemo:gate-complex-query 
								": binary gate in gate complex " (cn gate-complex)
								" must export an assignment"))
						(cons (list i-gate-var (cn i-gate-comp) ) ax))
					      ax)))
				      (list) gate-complexes)))

	   (for-each 
	    (lambda (a)
	      (let ((acc-ion   (car a)))
		(if (assoc acc-ion perm-ions)
		    (nemo:error 'nemo:gate-complex-query 
				": ion species " acc-ion " cannot be declared as both accumulating and permeating"))))
	    acc-ions)

	`((gate-complexes ,gate-complexes)
	  (perm-ions      ,perm-ions)
	  (acc-ions       ,acc-ions)
	  (mod-ions       ,mod-ions)
	  (pool-ions      ,pool-ions)
	  (i-gates        ,i-gates)
	  ))
      ))
  ))
	  
)
