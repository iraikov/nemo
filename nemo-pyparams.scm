;;       
;; 
;; An extension for generating Python code describing the parameters of NEMO models.
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

(module nemo-pyparams

	(nemo:pyparams-translator)

	(import scheme chicken utils data-structures lolevel ports srfi-1 srfi-13 srfi-69)
	
	(require-extension lolevel matchable strictly-pretty 
			   varsubst datatype nemo-core nemo-utils
			   nemo-gate-complex nemo-defaults nemo-geometry nemo-synapse)



(define (safe-car x)
  (and x (car x)))

(define (safe-cadr x)
  (and x (cadr x)))

(define (member-imports x imports)
  (safe-car (member x imports (lambda (x y) (equal? x (cadr y))))))


(define python-builtin-consts
  `())

(define python-ops
  `(+ - * / > < <= >= = ^))

(define builtin-fns
  `(+ - * / pow neg abs atan asin acos sin cos exp ln
      sqrt tan cosh sinh tanh hypot gamma lgamma log10 log2 log1p ldexp cube
      > < <= >= = and or round ceiling floor max min
      ))

(define (python-name s)
  (let ((cs (string->list (->string s))))
    (let loop ((lst (list)) (cs cs))
      (if (null? cs) (string->symbol (list->string (reverse lst)))
	  (let* ((c (car cs))
		 (c1 (cond ((or (char-alphabetic? c) (char-numeric? c) (char=? c #\_)) c)
			   (else #\_))))
	    (loop (cons c1 lst) (cdr cs)))))))
	    



(define (rhsexpr/python expr)
  (match expr 
	 (('if . es)  `(if . ,(map (lambda (x) (rhsexpr/python x)) es)))
	 (('pow x y)  (if (and (integer? y)  (positive? y))
			  (if (> y 1)  (let ((tmp (gensym "x")))
					 `(let ((,tmp ,x)) (* . ,(list-tabulate (inexact->exact y) (lambda (i) tmp)))))
			      x)
			    expr))
	 ((s . es)    (if (symbol? s)  (cons (if (member s builtin-fns) s (python-name s)) (map (lambda (x) (rhsexpr/python x)) es)) expr))
	 (id          (if (symbol? id) (python-name id) id))))


(define (python-state-name n s)
  (python-name (s+ n s)))


(define-syntax pp
  (syntax-rules ()
    ((pp indent val ...) (ppf indent (quasiquote val) ...))))


(define tuple/python   (doc:block 2 (doc:text "(") (doc:text ")")))
(define dict/python    (doc:block 2 (doc:text "{") (doc:text "}")))
(define group/python   (doc:block 2 (doc:text "(") (doc:text ")")))
(define block/python   (doc:block 2 (doc:empty) (doc:empty)))

(define (stmt/python x) 
  (match x
	 (($ doc 'DocCons _ ($ doc 'DocText "")) x)
	 (else  (doc:cons x (doc:text "")))))


(define (ifthen/python c e1 e2)
  (doc:nest 
   2 (doc:connect
      (doc:connect (doc:group (doc:connect (doc:text "if") c))
		   (doc:connect (doc:nest 2 e1)
				(doc:nest 2 (doc:connect (doc:text "else") e2))))
      (doc:text "end"))))

(define (letblk/python e1 e2)
  (cond ((equal? e1 (doc:empty)) (doc:group (doc:nest 2 e2)))
	((equal? e2 (doc:empty)) (doc:group (doc:nest 2 e1)))
	(else (doc:connect (doc:group (doc:nest 2 (stmt/python e1)))
			   (doc:group (doc:nest 2 e2))))))
	

(define (format-op/python indent op args)
  (let ((op1 (doc:text (->string op))))
    (if (null? args) op1
	(match args
	       ((x)      (doc:concat (list op1 x)))
	       ((x y)    (doc:concat (intersperse (list x op1 y) (doc:space))))
	       ((x y z)  (doc:concat (intersperse (list x op1 y op1 z) (doc:space))))
	       (lst      (let* ((n   (length lst))
				(n/2 (inexact->exact (round (/ n 2)))))
			   (doc:concat 
			    (intersperse 
			     (list (format-op/python indent op (take lst n/2 )) op1 
				   (format-op/python indent op (drop lst n/2 )))
			     (doc:space)))))))))


(define (format-fncall/python indent op args)
  (let ((op1 (doc:text (->string op))))
    (doc:cons op1 (group/python ((doc:list indent identity (lambda () (doc:text ", "))) args)))))


(define (format-dict/python indent args)
  (dict/python ((doc:list indent 
			  (lambda (x) 
			    (let ((k (car x)) (v (cdr x)))
			      (doc:cons (doc:text k) 
					(doc:cons (doc:text ": ") 
						  (doc:cons (doc:text v) 
							    (doc:empty))))))
			  (lambda () (doc:cons (doc:text ", ") 
					       (doc:cons (doc:break) 
							 (doc:empty)))))
		args)))

(define (format-tuple/python indent args)
  (tuple/python ((doc:list indent 
			  (lambda (v) (doc:text v))
			  (lambda () (doc:cons (doc:text ", ") 
					       (doc:cons (doc:break) 
							 (doc:empty)))))
		args)))


(define (name-normalize expr)
  (match expr 
	 (('if c t e)  `(if ,(name-normalize c) ,(name-normalize t) ,(name-normalize e)))
	 (('let bs e)
	  `(let ,(map (lambda (b) `(,(car b) ,(name-normalize (cadr b)))) bs) ,(name-normalize e)))
	 ((f . es) 
	  (cons f (map name-normalize es)))
	 ((? symbol? ) (python-name expr))
	 ((? atom? ) expr)))


(define (canonicalize-expr/python expr)
  (let ((subst-convert  (subst-driver (lambda (x) (and (symbol? x) x)) nemo:binding? identity nemo:bind nemo:subst-term)))
    (let* ((expr1 (if-convert expr))
	   (expr2 (subst-convert expr1 subst-empty))
	   (expr3 (let-lift expr2))
	   (expr4 (name-normalize expr3)))
      expr4)))


(define (format-expr/python indent expr . rest)  
  (let-optionals rest ((rv #f))
   (let ((indent+ (+ 2 indent)))
    (match expr
       (('let bindings body)
	(letblk/python
	 (fold-right 
	   (lambda (x ax)
	     (letblk/python
	      (match (second x)
		     (('if c t e)
		      (ifthen/python
		       (group/python (format-expr/python indent c))
		       (block/python (format-expr/python indent t (first x)))
		       (block/python (format-expr/python indent e (first x)))))
		     (else 
		      (stmt/python
		       (format-op/python indent+ " = "
					 (list (format-expr/python indent (first x) )
					       (format-expr/python indent (second x)))))))
	      ax))
	   (doc:empty) bindings)
	 (match body
		(('let _ _) (format-expr/python indent body rv))
		(else
		 (let ((body1 (doc:nest indent (format-expr/python indent body))))
		   (if rv (stmt/python (format-op/python indent " = " (list (format-expr/python indent+ rv ) body1)))
		       body1))))))
       
       (('if . rest) (error 'format-expr/python "invalid if statement " expr))

       ((op . rest)  
       (let ((op (case op ((pow)  '^)  ((ln) 'log) (else op))))
	 (let ((fe
		(if (member op python-ops)
		    (let ((mdiv?  (any (lambda (x) (match x (('* . _) #t) (('/ . _) #t) (else #f))) rest))
			  (mul?   (any (lambda (x) (match x (('* . _) #t) (else #f))) rest))
			  (plmin? (any (lambda (x) (match x (('+ . _) #t) (('- . _) #t) (else #f))) rest)))
		      (case op
			((/)  
			 (format-op/python indent op 
					  (map (lambda (x) 
						 (let ((fx (format-expr/python indent+ x)))
						   (if (or (symbol? x) (number? x)) fx
						       (if (or mul? plmin?) (group/python fx) fx)))) rest)))
			((*)  
			 (format-op/python indent op 
					  (map (lambda (x) 
						 (let ((fx (format-expr/python indent+ x)))
						   (if (or (symbol? x) (number? x)) fx
						       (if plmin? (group/python fx) fx)))) rest)))
			
			((^)  
			 (format-op/python indent op 
					  (map (lambda (x) 
						 (let ((fx (format-expr/python indent+ x)))
						   (if (or (symbol? x)  (number? x)) fx
						       (if (or mdiv? plmin?) (group/python fx) fx)))) rest)))
			
			(else
			 (format-op/python indent op 
					  (map (lambda (x) 
						 (let ((fx (format-expr/python indent+ x))) fx)) rest)))))
		    
		    (let ((op (case op ((neg) '-) (else op))))
		      (format-fncall/python indent op (map (lambda (x) (format-expr/python indent+ x)) rest))))))
	   (if rv 
	       (stmt/python (format-op/python indent " = " (list (format-expr/python indent+ rv ) fe)))
	       fe))))
      
      (else  (let ((fe (doc:text (->string expr))))
	       (if rv 
		   (stmt/python (format-op/python indent " = " (list (format-expr/python indent+ rv ) fe)))
		   fe)))))))

(define (doc->string x . rest)
  (let-optionals rest ((width 72))
   (sdoc->string (doc:format width x))))
	  
(define (expr->string/python x . rest)
  (let-optionals rest ((rv #f) (width 72))
     (doc->string
      (format-expr/python 2 x rv)
      width)))


(define (state-init n init)
  (let* ((init  (rhsexpr/python init))
	 (init1 (canonicalize-expr/python init)))
    (list (python-name n) init1)))


  
(define (make-define-fn)
  (lambda (indent n proc)
    (let ((lst (procedure-data proc))
	  (indent+ (+ 2 indent)))
      (let ((retval   (python-name (gensym "retval")))
	    (rt       (lookup-def 'rt lst))
	    (formals  (lookup-def 'formals lst))
	    (vars     (lookup-def 'vars lst))
	    (body     (lookup-def 'body lst)))
	(pp indent ,nl (function ,retval = ,(python-name n) (,(slp ", " vars)) ))
	(let* ((body0 (rhsexpr/python body))
	       (body1 (canonicalize-expr/python body0))
	       (lbs   (enum-bnds body1 (list))))
	  (pp indent+ ,(expr->string/python body1 retval))
	  (pp indent end))
	  ))))


(define (quantity-rhs quantity)
  (cases nemo:quantity quantity
         (CONST (name value u)  name)
         (PRIM (name value)  name)
         (ASGN (name value rhs u) rhs)
         (REACTION   (name initial open transitions conserve power u) open)
         (RATE (name initial rhs power u) rhs)
         (TRANSIENT  (name initial rhs asgn power u) rhs)
         (PRIM       (name value) name)
         (EXTERNAL   (local-name name namespace u) name)
         (else (nemo:error 'nemo:quantity-rhs ": unknown quantity type" quantity))
         ))




(define (output-pyparams sysname mode i-params i-eqs
			 const-defs asgn-eq-defs init-eq-defs 
			 pool-ions perm-ions mrc defaults geometry
                         indent indent+)

  (define (pystring s) (s+ "'" s "'"))
 
    
    (let* ((init-eqs 
	    (append 
	     
	     const-defs
	     asgn-eq-defs
	     init-eq-defs
	     
             (map (lambda (pool-ion)
                    (let ((n (pool-ion-in pool-ion))
                          (b (pool-ion-inq pool-ion)))
                      (list n b)))
                  pool-ions)))

	   (init-dag 
	    (map (lambda (def)
		   (cons (first def) (enum-freevars (second def) '() '())))
		 init-eqs))

	   (init-order
	    (reverse
	     (topological-sort init-dag 
	       (lambda (x y) (string=? (->string x) (->string y))))))

           (soma-geometry (lookup-def 'soma geometry))
           )

      (for-each (lambda (x)
		  (pp indent ,(expr->string/python (cadr x) (python-name (car x)))))
		const-defs)

      (pp indent ,nl)

      (let recur ((i-params i-params) 
		  (property-tuples '() ))

	(if (null? i-params)
	    
            (let ((property-tuples1
                   (append
                    (map (match-lambda ((x v) (list (list "Extracellular" x) (pystring (python-name x)) (->string (rhsexpr/python v))))) defaults)
                    (or (and mrc
                             (let ((cm (->string (python-name mrc))))
                               `((("Membrane" "cm") ,(pystring cm) ,cm))))
                        '())
                    (or (and soma-geometry
                             (let ((L (->string (python-name (second (first soma-geometry)))))
                                   (diam (->string (python-name (second (second soma-geometry))))))
                             `((("Geometry" "L") ,(pystring L) ,L)
                               (("Geometry" "diam") ,(pystring diam) ,diam))))
                        '())
                    property-tuples))
                  )
              (pp indent (,(doc->string 
			  (format-op/python 
			   indent " = "
			   (list (format-expr/python indent "properties" )
				 (format-dict/python 
				  indent
				  (map (lambda (t)
                                         (let ((k (if (string? (car t))
                                                      (pystring (car t))
                                                      (s+ "(" (slp ", " (map (lambda (x) (pystring x)) (car t))) ")") ))
                                               )
                                           `(,k . ,(doc->string (format-tuple/python indent+ (cdr t))))
                                           ))
				       property-tuples1)
				  ))
			   ))
			))
              )

	    (let ((paramset (car i-params)))
	      (let ((alst (cdr paramset)))
		(let ((label (lookup-def 'label alst))
		      (maximal-permeability (lookup-def 'maximal-permeability alst))
		      (maximal-conductance  (lookup-def 'maximal-conductance alst))
		      (reversal-potential   (lookup-def 'reversal-potential alst)))
		  (recur (cdr i-params)
			 (let* ((property-tuples1 
				 (fold (lambda (l x ax)
					 (or (and x (cons `(,l ,(s+ "'" (python-name x) "'")
							       ,(symbol->string (python-name x)) 
							       ) 
							  ax))
					     ax))
				       property-tuples
                                       (list (list label "pbar")
                                             (list label "gbar")
                                             (list label "e_rev"))
				       (list maximal-permeability
					     maximal-conductance 
					     reversal-potential)
				       ))
                                )
			   property-tuples1))
		  )))
	    ))

    ))

(define (pyparams-translator1 sys . rest)
  (define (cid x)  (second x))
  (define (cn x)   (first x))
  (let-optionals rest ((mode 'multiple) (filename #f))
  (match-let ((($ nemo:quantity 'DISPATCH  dis) (hash-table-ref sys (nemo-intern 'dispatch))))
    (let ((imports  ((dis 'imports)  sys))
	  (exports  ((dis 'exports)  sys)))
      (let* ((indent      0)
	     (indent+     (+ 2 indent ))
	     (sysname     (python-name ((dis 'sysname) sys)))
	     (prefix      sysname)
	     (filename    (or filename (s+ sysname ".py")))
	     (deps*       ((dis 'depgraph*) sys))
	     (consts      ((dis 'consts)  sys))
	     (asgns       ((dis 'asgns)   sys))
	     (states      ((dis 'states)  sys))
	     (reactions   ((dis 'reactions) sys))
	     (defuns      ((dis 'defuns)  sys))
	     (components  ((dis 'components) sys))
	     
	     (g             (match-let (((state-list asgn-list g) ((dis 'depgraph*) sys))) g))
	     (poset         (vector->list ((dis 'depgraph->bfs-dist-poset) g)))

	     (const-defs       (filter-map
				(lambda (nv)
				  (and (not (member (first nv) python-builtin-consts))
				       (let ((v1 (canonicalize-expr/python (second nv))))
					 (list (python-name (first nv)) v1))))
				consts))
	     
	     (gate-complex-info    (nemo:gate-complex-query sys))
	     (defaults             (nemo:defaults-query sys))
             (geometry             (nemo:geometry-query sys))

	     (gate-complexes       (lookup-def 'gate-complexes gate-complex-info))
	     (perm-ions            (map (match-lambda ((comp i e erev val) `(,comp ,(python-name i) ,(python-name e) ,erev)))
                                        (lookup-def 'perm-ions gate-complex-info)))
	     (acc-ions             (map (match-lambda ((comp i in out) `(,comp ,@(map python-name (list i in out)))))
                                        (lookup-def 'acc-ions gate-complex-info)))
	     (epools               (lookup-def 'pool-ions gate-complex-info))
	     (pool-ions            (pool-ion-name-map python-name epools))
             
	     (i-gates              (lookup-def 'i-gates gate-complex-info))
	     (synapse-info         (nemo:post-synaptic-conductance-query sys))
             (isyns                (lookup-def 'i-synapses synapse-info))
             (pscs                 (lookup-def 'post-synaptic-conductances synapse-info))

	     (comprc               (any (match-lambda ((name 'membrane-tau id) (list name id)) (else #f)) components))
	     (compcap              (any (match-lambda ((name 'membrane-capacitance id) (list name id)) (else #f)) components))
	     (mrc                  (or (and comprc (car ((dis 'component-exports) sys (cid comprc))))
                                       (lookup-def 'membrane-tau defaults)
                                       (lookup-def 'tau_m defaults)
                                       (and compcap (car ((dis 'component-exports) sys (cid compcap))))
                                       (lookup-def 'membrane-capacitance defaults)
                                       (lookup-def 'C_m defaults)
                                       ))

	     (i-eqs+params
	      (filter-map
		     (lambda (gate-complex) 
		       
		       (let* (
                              (label             (first gate-complex))
			      (n                 (second gate-complex))
			      (subcomps          ((dis 'component-subcomps) sys n))
			      (acc               (lookup-def 'accumulating-substance subcomps))
			      (perm              (lookup-def 'permeating-ion subcomps))
			      (permqs            (and perm ((dis 'component-exports) sys (cid perm))))
			      (pore              (lookup-def 'pore subcomps))
			      (permeability      (lookup-def 'permeability subcomps))
			      (gate              (lookup-def 'gate subcomps))
			      (sts               (and gate ((dis 'component-exports) sys (cid gate))))
                              )
			 
			 (if (not (or pore permeability))
			     (nemo:error 'nemo:python-translator ": ion channel definition " label
					 "lacks any pore or permeability components"))


			 (cond ((and perm permeability gate)
                                (let* ((i     (python-name (s+ 'i (cn perm))))
                                       (pmax  (car ((dis 'component-exports) sys (cid permeability))))
                                       (pbar  (cadr ((dis 'component-exports) sys (cid permeability))))
                                       (pwrs  (map (lambda (n) (state-power sys n)) sts))
                                       (sptms (map (lambda (st pwr) `(pow ,st ,pwr)) sts pwrs))
                                       (gion  `(* ,pmax ,@sptms))
                                       (pbar-quantity (hash-table-ref sys pbar))
                                       )
                                    (list i #f gion (python-name (s+ 'i_ label) )
                                          `((label . ,label) 
                                            (maximal-permeability . ,(quantity-rhs pbar) ))))
                                )

			       ((and perm pore gate)
				(case (cn perm)
				  ((non-specific)
				   (let* ((i     (python-name 'i))
					  (e     (car permqs))
					  (gmax  (car ((dis 'component-exports) sys (cid pore))))
					  (pwrs  (map (lambda (n) (state-power sys n)) sts))
					  (sptms (map (lambda (st pwr) `(pow ,st ,pwr)) sts pwrs))
					  (gion  `(* ,gmax ,@sptms))
                                          (gmax-quantity (hash-table-ref sys gmax))
                                          (e-quantity (hash-table-ref sys e))
					  )

				     (list i e gion  (python-name (s+ 'i_ label))
					   `((label . ,label)
					     (maximal-conductance . ,(quantity-rhs gmax-quantity))
					     (reversal-potential . ,(quantity-rhs e-quantity)))
					   )))

				  (else
				   (let* ((i     (python-name (s+ 'i (cn perm))))
					  (e     (car permqs))
					  (gmax  (car ((dis 'component-exports) sys (cid pore))))
					  (pwrs  (map (lambda (n) (state-power sys n)) sts))
					  (sptms (map (lambda (st pwr) `(pow ,st ,pwr)) sts pwrs))
					  (gion  `(* ,gmax ,@sptms))
				     
					  ;; this handles a special case when a mechanism is defined as 
					  ;; ohmic (i.e. with reversal potential), but uses ionic 
					  ;; concentrations to compute reversal potential; 
					  ;; in this case, the reversal potential is computed 
					  ;; in the mechanism at each timestep and it is not 
					  ;; a user-settable parameter... 
					  
					  (ion     (cn perm))
					  (concqs  (filter identity
							   (list (safe-cadr (member-imports (string->symbol (s+ ion 'i)) imports))
								 (safe-cadr (member-imports (string->symbol (s+ ion 'o)) imports)))))
					  (gmax-quantity  (hash-table-ref sys gmax))
					  (e-quantity  (hash-table-ref sys e))
					  )

				     (if (null? concqs)
					 (list i e gion (python-name (s+ 'i_ label))
					       `((label . ,label)
						 (maximal-conductance . ,(quantity-rhs gmax-quantity))
						 (reversal-potential . ,(quantity-rhs e-quantity))))
					 (list i e gion (python-name (s+ 'i_ label))
					       `((label . ,label)
						 (maximal-conductance . ,(quantity-rhs gmax-quantity)))
					 ))
				   ))
				))
			       
			       ((and perm pore)
				(case (cn perm)
				  ((non-specific)
				   (let* ((i     (python-name 'i))
					  (e     (car permqs))
					  (gmax  (car ((dis 'component-exports) sys (cid pore))))
                                          (gmax-quantity (hash-table-ref sys gmax ))
                                          (e-quantity (hash-table-ref sys e ))
                                          )
				     (list i e gmax (python-name (s+ 'i_ label))
					   `((label . ,label)
					     (maximal-conductance . ,(quantity-rhs gmax-quantity))
					     (reversal-potential . ,(quantity-rhs e-quantity)))
					   )))
				  (else
				   (nemo:error 'nemo:python-translator ": invalid ion channel definition " label))))
			       
			       ((and acc pore gate)
				(let* ((i     (python-name (s+ 'i (cn acc))))
				       (gmax  (car ((dis 'component-exports) sys (cid pore))))
				       (pwrs  (map (lambda (n) (state-power sys n)) sts))
				       (sptms (map (lambda (st pwr) `(pow ,st ,pwr)) sts pwrs))
				       (gion  `(* ,gmax ,@sptms))
                                       (gmax-quantity (hash-table-ref sys gmax)))
				  (list i #f gion  (python-name (s+ 'i_ label))
					   `((label . ,label)
					     (maximal-conductance . ,(quantity-rhs gmax-quantity))))
					))
			       (else (nemo:error 'nemo:python-translator ": invalid ion channel definition " label))
			       )))
		     gate-complexes))


	     (i-params (append 
                        (map (lambda (i-eq) (cons (car i-eq) (cadr (cdddr i-eq)))) i-eqs+params)
                        (map (lambda (isyn psc)
                               `(,(python-name (s+ 'i_ (first psc)))
                                 (label . ,(first psc))
                                 (reversal-potential . ,(third isyn)))
                               )
                             isyns pscs))
                       )


	     (i-eqs    (map (lambda (i-eq) (take i-eq 4)) i-eqs+params))

	     (i-names (delete-duplicates (map first i-eqs)))
		
	     (i-eqs  (fold  (lambda (i-gate ax) 
			      (let ((i-gate-var (first i-gate)))
				(cons (list (python-name 'i) #f i-gate-var (s+ 'i_ (second i-gate))) ax)))
			    i-eqs i-gates))

             (i-eqs (fold (lambda (isyn psc ax)
                            (cons (list (first isyn) (third isyn) (second isyn) (s+ 'i_ (first psc))) ax))
                          i-eqs isyns pscs))

	     (i-bkts (bucket-partition (lambda (x y) (eq? (car x) (car y))) i-eqs))
	     
	     (i-eqs  (fold (lambda (b ax) 
			     (match b 
				    ((and ps ((i e gion ii) . rst))  
				     (let loop ((ps ps) (summands (list)) (eqs (list)))
				       (if (null? ps)
					   
					   (let* ((sum0  (sum summands))
						  (sum1  (rhsexpr/python sum0))
						  (sum2  (canonicalize-expr/python sum1)))
					     (append eqs (list (list i sum2)) ax))
					   
					   (match-let (((i e gion ii) (car ps)))
						      (loop (cdr ps) 
							    (cons ii summands) 
							    (let* ((expr0 (rhsexpr/python (if e `(* ,gion (- v ,e)) gion)))
								   (expr1 (canonicalize-expr/python expr0)))
							      (cons (list ii expr1) eqs)))))))
				    
				    ((i e gion ii)
				     (let* ((expr0  (rhsexpr/python (if e `(* ,gion (- v ,e)) gion)))
					    (expr1  (canonicalize-expr/python expr0)))
				       (cons (list i expr1) ax)))
				    
				    (else ax)))
			   (list) i-bkts))

	     (asgn-eq-defs       (poset->asgn-eq-defs* poset sys python-name rhsexpr/python canonicalize-expr/python builtin-fns))
	     
	     (rate-eq-defs       (reverse (poset->rate-eq-defs* poset sys #f python-name python-state-name rhsexpr/python canonicalize-expr/python builtin-fns)))
	     
	     (reaction-eq-defs   (poset->reaction-eq-defs* poset sys python-name python-state-name rhsexpr/python canonicalize-expr/python))
	     
	     (init-eq-defs       (poset->init-defs* poset sys python-name python-state-name rhsexpr/python canonicalize-expr/python builtin-fns))
	     
	     (conserve-eq-defs   (map (lambda (eq) (list 0 `(- ,(second eq) ,(first eq)))) 
				      (poset->state-conserve-eq-defs poset sys python-name python-state-name)))
	     
	     (v-eq    (if mrc 
			  (list 'v (rhsexpr/python `(/ (neg ,(sum i-names)) ,mrc)))
			  (list 'v 0.0)))
	     
	     (dfenv 
	      (map (lambda (x) (let ((n (first x)))
				 (list n (python-name (s+ "d_" n )))))
		   defuns))

	     )

	
	(for-each 
	 (lambda (a)
	   (let ((acc-ion   (car a)))
	     (if (assoc acc-ion perm-ions)
		 (nemo:error 'nemo:python-translator 
			     ": ion species " acc-ion " cannot be declared as both accumulating and permeating"))))
	 acc-ions)

        (let ((output (open-output-file filename)))
          
          (with-output-to-port output
            (lambda ()
              (output-pyparams sysname mode i-params i-eqs const-defs
                               asgn-eq-defs init-eq-defs pool-ions perm-ions
                               mrc defaults geometry indent indent+)
              (pp indent ,nl)))
          
          (if output (close-output-port output)))

        ))
    ))
  )

(define (nemo:pyparams-translator syss . rest)
  (let-optionals rest ((mode 'multiple) (filename #f))
    (close-output-port (open-output-file filename))
    (for-each 
     (lambda (sys)
       (pyparams-translator1 sys mode filename))
     syss)))


)
