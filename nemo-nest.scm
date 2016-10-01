;;       
;; An extension for translating NEMO models to NEST code.
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

(module nemo-nest

	(nemo:nest-translator)

	(import scheme chicken utils data-structures ports extras srfi-1 srfi-13 srfi-69)
	
	(require-extension lolevel posix matchable strictly-pretty 
			   varsubst datatype nemo-core nemo-utils nemo-units
                           nemo-geometry nemo-defaults nemo-constraints
			   nemo-gate-complex nemo-synapse nemo-currents

                           )
	(require-library ersatz-lib)

        (import (prefix ersatz-lib ersatz: ))




(define C++-ops
  `(+ - * / > < <= >= =))


(define (nest-name s)
  (let ((cs (string->list (->string s))))
    (let loop ((lst (list)) (cs cs))
      (if (null? cs) (string->symbol (list->string (reverse lst)))
	  (let* ((c (car cs))
		 (c1 (cond ((or (char-alphabetic? c) (char-numeric? c) (char=? c #\_)) c)
			   (else #\_))))
	    (loop (cons c1 lst) (cdr cs)))))))


(define builtin-consts
  (append `(params)
          (map (lambda (x) (nest-name (s+ "M_" (first x)))) nemo:math-constants)))


(define builtin-fns
  `(+ - * / pow neg abs atan asin acos sin cos exp ln
      sqrt tan cosh sinh tanh hypot gamma lgamma log10 log2 log1p ldexp cube
      > < <= >= = and or round ceiling floor max min
      ))


(define (rewrite-pow expr)
  (match expr
         (('pow x y)  (if (and (integer? y)  (positive? y))
			  (if (> y 1)  (let ((tmp (gensym "x")))
					 `(let ((,tmp ,x)) (* . ,(list-tabulate (inexact->exact y) (lambda (i) tmp)))))
			      x)
			  (if (and (number? y) (zero? y)) 1.0 expr)))
         (else expr)))

	    
(define (rhsexpr/C++ expr)
  (match expr 
	 (('if . es)  `(if . ,(map (lambda (x) (rhsexpr/C++ x)) es)))
	 (('pow x y)  (cond ((and (integer? y) (= y 1)) x)
                            ((and (number? y) (zero? y)) 1.0)
                            (else expr)))
	 ((s . es)    (if (symbol? s)  (cons (if (member s builtin-fns) s (nest-name s))
					     (map (lambda (x) (rhsexpr/C++ x)) es)) expr))
	 (id          (if (symbol? id) 
                          (if (assoc id nemo:math-constants)
                              (nest-name (s+ "M_" id))
                              (nest-name id))
                          id))
         ))


(define (nest-state-name n s)
  (nest-name (s+ n s)))


(define-syntax pp
  (syntax-rules ()
    ((pp indent val ...) (ppf indent (quasiquote val) ...))))


(define group/C++   (doc:block 2 (doc:text "(") (doc:text ")")))
(define block/C++   (doc:block 2 (doc:text "{") (doc:text "}")))
(define (stmt/C++ x) 
  (match x
	 (($ doc 'DocCons _ ($ doc 'DocText ";")) x)
	 (else  (doc:cons x (doc:text ";")))))


(define (ifthen/C++ c e1 e2)
  (doc:nest 2
    (doc:connect (doc:group (doc:connect (doc:text "if") c))
		 (doc:connect (doc:nest 2 e1)
			      (doc:nest 2 (doc:connect 
					   (doc:text "else") 
					   e2))))
    ))


(define (letblk/C++ e1 e2)
  (cond ((equal? e1 (doc:empty)) (doc:group (doc:nest 2 e2)))
	((equal? e2 (doc:empty)) (doc:group (doc:nest 2 e1)))
	(else (doc:connect (doc:group (doc:nest 2 (stmt/C++ e1)))
			   (doc:group (doc:nest 2 e2))))))
	

(define (format-op/C++ indent op args)
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
			     (list (format-op/C++ indent op (take lst n/2 )) op1 
				   (format-op/C++ indent op (drop lst n/2 )))
			     (doc:space)))))))))


(define (format-fncall/C++ indent op args)
  (let ((op1 (doc:text (->string op))))
    (doc:cons op1 (group/C++ ((doc:list indent identity (lambda () (doc:text ", "))) args)))))


(define (name-normalize expr)
  (match expr 
	 (('if c t e)  `(if ,(name-normalize c) ,(name-normalize t) ,(name-normalize e)))
	 (('let bs e)
	  `(let ,(map (lambda (b) `(,(car b) ,(name-normalize (cadr b)))) bs) ,(name-normalize e)))
	 ((f . es) 
	  (cons (if (member f builtin-fns) f (nest-name f)) (map name-normalize es)))
	 ((? symbol? ) (nest-name expr))
	 ((? atom? ) expr)))


(define (canonicalize-expr/C++ expr)
  (let ((subst-convert  (subst-driver (lambda (x) (and (symbol? x) x)) nemo:binding? identity nemo:bind nemo:subst-term)))
    (let* ((expr1 (if-convert expr))
	   (expr2 (subst-convert expr1 subst-empty))
	   (expr3 (let-lift expr2))
	   (expr4 (name-normalize expr3)))
      expr4)))


(define (format-expr/C++ indent expr . rest)  
  (let-optionals rest ((rv #f))
   (let ((indent+ (+ 2 indent)))
    (match expr
       (('let bindings body)
	(letblk/C++
	 (fold-right 
	   (lambda (x ax)
	     (letblk/C++
	      (match (second x)
		     (('if c t e)
		      (ifthen/C++
		       (group/C++ (format-expr/C++ indent c))
		       (block/C++ (format-expr/C++ indent t (first x)))
		       (block/C++ (format-expr/C++ indent e (first x)))))
		     (else 
		      (stmt/C++
		       (format-op/C++ indent+ " = "
					 (list (format-expr/C++ indent (first x) )
					       (format-expr/C++ indent (second x)))))))
	      ax))
	   (doc:empty) bindings)
	 (match body
		(('let _ _) (format-expr/C++ indent body rv))
		(else
		 (let ((body1 (doc:nest indent (format-expr/C++ indent body))))
		   (if rv (stmt/C++ (format-op/C++ indent " = " (list (format-expr/C++ indent+ rv ) body1)))
		       body1))))))
       
       (('if . rest) (error 'format-expr/C++ "invalid if statement " expr))

       ((op . rest)  
       (let ((op (case op ((ln) 'log) ((abs) 'fabs) (else op))))
	 (let ((fe
		(if (member op C++-ops)
		    (let ((mdiv?  (any (lambda (x) (match x (('* . _) #t) (('/ . _) #t) (else #f))) rest))
			  (mul?   (any (lambda (x) (match x (('* . _) #t) (else #f))) rest))
			  (plmin? (any (lambda (x) (match x (('+ . _) #t) (('- . _) #t) (else #f))) rest)))
		      (case op
			((/)  
			 (format-op/C++ indent op 
					  (map (lambda (x) 
						 (let ((fx (format-expr/C++ indent+ x)))
						   (if (or (symbol? x) (number? x)) fx
						       (if (or mul? plmin?) (group/C++ fx) fx)))) rest)))
			((*)  
			 (format-op/C++ indent op 
					  (map (lambda (x) 
						 (let ((fx (format-expr/C++ indent+ x)))
						   (if (or (symbol? x) (number? x)) fx
						       (if plmin? (group/C++ fx) fx)))) rest)))
			
			(else
			 (format-op/C++ indent op 
					  (map (lambda (x) 
						 (let ((fx (format-expr/C++ indent+ x))) fx)) rest)))))
		    
		    (let ((op (case op ((neg) '-) (else op))))
		      (format-fncall/C++ indent op (map (lambda (x) (format-expr/C++ indent+ x)) rest))))))
	   (if rv 
	       (stmt/C++ (format-op/C++ indent " = " (list (format-expr/C++ indent+ rv ) fe)))
	       fe))))
      
      (else  (let ((fe (doc:text (->string expr))))
	       (if rv 
		   (stmt/C++ (format-op/C++ indent " = " (list (format-expr/C++ indent+ rv ) fe)))
		   fe)))))))
	       
	  
(define (expr->string/C++ x . rest)
  (let-optionals rest ((rv #f) (width 72))
    (sdoc->string (doc:format width (format-expr/C++ 2 x rv)))))



(define nest-template 
  (ersatz:statements-from-file 
   (ersatz:template-std-env 
    search-path: `(,template-dir))
   "NEST.tmpl"))

(define nest-header-template 
  (ersatz:statements-from-file 
   (ersatz:template-std-env 
    search-path: `(,template-dir))
   "NEST-header.tmpl"))



(define (make-define-fn sysname )
  (lambda (indent n proc)

    (let* (
           (lst      (procedure-data proc))
           (indent+  (+ 2 indent))
           (rt       (or (lookup-def 'rt lst) 'double))
           (formals  (lookup-def 'formals lst))
           (vars     (lookup-def 'vars lst))
           (consts   (filter (lambda (x) (not (procedure? (cdr x)))) (lookup-def 'consts lst)))
           (body     (lookup-def 'body lst))
           (rv       (gensym 'rv))
           (body0    (rhsexpr/C++ body))
           (body1    (canonicalize-expr/C++ (add-params-to-fncall body0 builtin-fns)))
           (lbs      (enum-bnds body1 (list)))
           (args     (append
                      (if (null? vars) '() (map (lambda (x) (sprintf "double ~A" (nest-name x))) vars))
                      '("const void* params")))
           )

        (let (
              (tmpl-env
               (fold tenv-enter '()
                     `(
                       (name . ,(nest-name n))
                       (vars . ,args)

                       (localVars    . ,(if (null? lbs) (ersatz:Tlist '()) (ersatz:sexpr->tvalue lbs)))
                       (exprString   . ,(ersatz:Tstr (expr->string/C++ body1 (nest-name rv))))
                       
                       (returnType . ,rt)
                       (returnVar  . ,rv)
                       
                       (consts . ,(map (compose nest-name car) consts))
                       ))
               )
              )

          tmpl-env

          ))
    ))


(define (ith v i) (sprintf "Ith(~A,~A)" v i))



(define (nemo:nest-translator sys . rest)

  (define (cid x)  (second x))
  (define (cn x)   (first x))


  (let-optionals rest ((dirname ".")  
                       (method #f) (ss-method #f) 
                       (abstol #f) (reltol #f) (maxstep #f)
                       (dump-template-env #f))

    (let ((method (or method 'gsl)))

      (if (not (member method '(gsl cvode ida)))
	  (nemo:error 'nemo:nest-translator ": unknown method " method))

  (match-let ((($ nemo:quantity 'DISPATCH  dis) (hash-table-ref sys (nemo-intern 'dispatch))))
    (let (
          (getstate (case method
                      ((cvode ida) (lambda (i) (sprintf "Ith(B_.y,~A)" i)))
                      (else (lambda (i) (sprintf "Ith(S_.y_,~A)" i)))))
          (imports  ((dis 'imports)  sys))
	  (exports  ((dis 'exports)  sys))
          )
      (let* ((indent      0)
	     (indent+     (+ 2 indent ))

	     (sysname     (nest-name ((dis 'sysname) sys)))
	     (prefix      (->string sysname))
	     (deps*       ((dis 'depgraph*)  sys))
	     (consts      ((dis 'consts)     sys))
	     (asgns       ((dis 'asgns)      sys))
	     (states      ((dis 'states)     sys))
	     (reactions   ((dis 'reactions)  sys))
	     (defuns      ((dis 'defuns)     sys))
	     (components  ((dis 'components) sys))
	     
	     (g             (match-let (((state-list asgn-list g) ((dis 'depgraph*) sys))) g))
	     (poset         (vector->list ((dis 'depgraph->bfs-dist-poset) g)))

	     (const-defs       (filter-map
				(lambda (nv)
				  (and (not (member (first nv) builtin-consts))
				       (let ((v1 (canonicalize-expr/C++ (second nv))))
					 (list (nest-name (first nv)) v1))))
				consts))
	     
	     (defaults             (nemo:defaults-query sys))

             (geometry             (nemo:geometry-query sys))

	     (gate-complex-info    (nemo:gate-complex-query sys))
	     (perm-ions       (map (match-lambda ((comp i e erev val) `(,comp ,(nest-name i) ,(nest-name e) ,erev)))
				   (lookup-def 'perm-ions gate-complex-info)))
	     (acc-ions        (map (match-lambda ((comp i in out) `(,comp ,@(map nest-name (list i in out)))))
				   (lookup-def 'acc-ions gate-complex-info)))
	     (epools          (lookup-def 'pool-ions gate-complex-info))
	     (pool-ions       (pool-ion-name-map nest-name  epools))

	     (comprc         (any (match-lambda ((name 'membrane-tau id) (list name id)) (else #f)) components))
	     (compcap        (any (match-lambda ((name 'membrane-capacitance id) (list name id)) (else #f)) components))
	     (mrc            (or (and comprc (car ((dis 'component-exports) sys (cid comprc))))
                                 (lookup-def 'membrane-tau defaults)
                                 (lookup-def 'tau_m defaults)
				 (and compcap (car ((dis 'component-exports) sys (cid compcap))))
                                 (lookup-def 'membrane-capacitance defaults)
                                 (lookup-def 'C_m defaults)
                                 ))

	     (soma-geometry  (lookup-def 'soma geometry))
	     (marea          (and soma-geometry (third soma-geometry)))

	     (gate-complexes       (lookup-def 'gate-complexes gate-complex-info))
	     (synapse-info         (nemo:post-synaptic-conductance-query sys))

             (pscs           (lookup-def 'post-synaptic-conductances synapse-info))
             (psc-transients (map (lambda (lst) (map nest-name lst)) 
                                  (lookup-def 'psc-transients synapse-info)))

	     (i-syns         (lookup-def 'i-synapses synapse-info))
		
	     (i-gates        (lookup-def 'i-gates gate-complex-info))

	     (i-defs         (nemo:ionic-current-definitions
                              gate-complexes i-gates i-syns pscs marea
                              (lambda (x) (state-power sys x))
                              (lambda (x) ((dis 'component-exports) sys x))
                              (lambda (x) ((dis 'component-subcomps) sys x))
                              nest-name rhsexpr/C++ canonicalize-expr/C++
                              builtin-fns))

             (i-eqs          (lookup-def 'i-eqs i-defs))
             (i-names        (lookup-def 'i-names i-defs))

             (constraints    (nemo:constraint-definitions 
                              gate-complexes i-gates i-syns pscs marea imports
                              (lambda (x) (state-power sys x))
                              (lambda (x) (quantity-unit sys x))
                              (lambda (x) ((dis 'component-exports) sys x))
                              (lambda (x) ((dis 'component-subcomps) sys x))
                              nest-name))

	     (v-eq    (and (not (null? i-names))
			   (let ((istim "(node.B_.I_stim_)" )) 
			     (cond

                              ((and mrc marea)
                               (list 'v (rhsexpr/C++ 
                                         `(/ (+ (* ,istim (/ 100. ,marea)) 
                                                (* -1e3 ,(sum i-names))) ,mrc))))
                              (marea
                               (list 'v (rhsexpr/C++ 
                                         `(+ (* ,istim (/ 100. ,marea))
                                             (* -1e-3 ,(sum i-names))))))
                              (mrc
                               (list 'v (rhsexpr/C++ `(/ (+ ,istim (* -1e-3 ,(sum i-names))) ,mrc))))
                              
                              (else
                               (list 'v (rhsexpr/C++ `(+ ,istim (* -1e-3 ,(sum i-names))))))
                              ))
                           ))

             (v-eq
              (and v-eq
                   (list (first v-eq) 
                         (add-params-to-fncall (canonicalize-expr/C++ (second v-eq)) builtin-fns))))

	     (external-eq-defs   (sys->external-eq-defs
                                  sys nest-name rhsexpr/C++ canonicalize-expr/C++
                                  namespace-filter: (lambda (x) (not (equal? x 'event)))))

	     (event-external-eq-defs (sys->external-eq-defs 
                                      sys nest-name rhsexpr/C++ canonicalize-expr/C++
                                      namespace-filter: (lambda (x) (equal? x 'event))))

	     (asgn-eq-defs       (poset->asgn-eq-defs* 
                                  poset sys nest-name rhsexpr/C++ canonicalize-expr/C++ builtin-fns))
	     
	     (rate-eq-defs       (let ((eqs0 (poset->rate-eq-defs* 
                                              poset sys method nest-name nest-state-name 
                                              rhsexpr/C++ canonicalize-expr/C++ builtin-fns)))

                                   (if v-eq
                                       (cons v-eq (reverse eqs0))
                                       (reverse eqs0))))

	     (state-index-map  (let ((acc (fold (lambda (def ax)
						  (let ((st-name (first def)))
						    (list (+ 1 (first ax)) 
							  (cons `(,st-name ,(first ax)) (second ax)))))
						(list 0 (list)) 
                                                rate-eq-defs
						)))
				 (second acc)))
	     
	     
	     (reaction-eq-defs   (poset->reaction-eq-defs* 
                                  poset sys nest-name nest-state-name rhsexpr/C++ canonicalize-expr/C++))

	     (transient-event-defs  (poset->transient-event-defs 
                                     poset sys method nest-name nest-state-name rhsexpr/C++ canonicalize-expr/C++ builtin-fns)) 
	     
	     (init-eq-defs       (let ((eqs0 (poset->init-defs* poset sys nest-name nest-state-name
                                                                rhsexpr/C++ canonicalize-expr/C++ builtin-fns))
                                       (vi (lookup-def 'v state-index-map))
                                       (vrest (or (and (lookup-def 'Vrest const-defs) 'Vrest) -65.0)))
                                     (if (and vi vrest) 
                                         (cons (list 'v vrest) eqs0)
                                         eqs0)))

	     (steady-state-index-map  (let ((acc (fold
                                                  (lambda (def ax)
                                                    (let ((st-name (first def)))
                                                      (if (not (alist-ref st-name init-eq-defs))
                                                          (list (+ 1 (first ax)) 
                                                                (cons `(,st-name ,(first ax)) (second ax)))
                                                          ax)))
                                                  (list 0 (list)) 
                                                  rate-eq-defs)))
					(second acc)))
             
	     (conserve-eq-defs   (map (lambda (eq) (list 0 `(- ,(second eq) ,(first eq)))) 
				      (poset->state-conserve-eq-defs poset sys nest-name nest-state-name)))
	     
	     (imports-sans-v (filter (lambda (x) (not (equal? 'v (first x)))) imports))

             (c-eqs (lookup-def 'c-eqs constraints))

             (c-units (map (lambda (x) 
                             (let ((n (first x)) (v (second x)))
                               (list (nest-name n) v)))
                           (lookup-def 'c-units constraints)))
             
             (i-eqs 
              (map 
               (lambda (def) (list (first def) 
                                   (add-params-to-fncall (canonicalize-expr/C++ (second def)) builtin-fns)))
               i-eqs))

             (init-eqs 
              (append 
               
               (map (lambda (def)
                      (let ((n (first def))
                            (b (second def)))
                        (list (nest-name n) (nest-name b))))
                    external-eq-defs)
               
               asgn-eq-defs
               init-eq-defs
               
               (map (lambda (pool-ion)
                      (let ((n (pool-ion-in pool-ion))
                            (b (pool-ion-inq pool-ion)))
                        (list n b)))
                    pool-ions)
               
               (map (lambda (pool-ion)
                      (let ((n (pool-ion-out pool-ion))
                            (b (pool-ion-outq pool-ion)))
                        (list n b)))
                    pool-ions)
               ))


             (init-dag 
              (map (lambda (def)
                     (cons (first def) (enum-freevars (second def) '() '())))
                   init-eqs))
             
             (init-order
              (reverse
               (topological-sort init-dag 
                                 (lambda (x y) (string=? (->string x) (->string y))))))

             (init-locals  (find-locals (map second (append init-eqs i-eqs reaction-eq-defs))))
             
             (init-vars (delete-duplicates
                         (map ->string 
                              (filter (lambda (x) (not (member x builtin-consts)))
                                      (append 
                                       init-locals
                                       init-order
                                       (map first external-eq-defs)
                                       (map pool-ion-in pool-ions)
                                       (map pool-ion-out pool-ions)
                                       (map first i-eqs)
                                       (map first steady-state-index-map) 
                                       (map first const-defs)
                                       (map first reaction-eq-defs)
                                       )))
                         string=?))
             
             (ss-get-state-defs
              (case ss-method
                ((kinsol)
                 (filter-map 
                  (lambda (def)
                    (let* ((n   (first def)) 
                           (ni  (lookup-def n steady-state-index-map)))
                      (and ni (expr->string/C++ (ith 'u ni) n))
                      ))
                  rate-eq-defs))
                (else
                 (filter-map 
                  (lambda (def)
                    (let* ((n   (first def)) 
                           (ni  (lookup-def n steady-state-index-map)))
                      (and ni (expr->string/C++ (sprintf "gsl_vector_get (u, ~A)" ni) n))
                      ))
                  rate-eq-defs))
                ))
             
             (ss-set-state-defs+lbs
              (case ss-method
                ((kinsol)
                 (filter-map 
                  (lambda (def)
                    (let* ((n   (first def)) 
                           (ni  (lookup-def n steady-state-index-map))
                           (b   (second def))
                           (lbs (delete-duplicates (find-locals (list b)))))
                      (and ni (list (list (expr->string/C++ b (ith 'f ni))) lbs))
                      ))
                  rate-eq-defs))
                (else
                 (filter-map 
                  (lambda (def)
                    (let* ((n   (first def)) 
                           (ni  (lookup-def n steady-state-index-map))
                           (b   (second def))
                           (lbs (delete-duplicates (find-locals (list b)))))
                      (and ni 
                           (let ((tmp (gensym 't)))
                             (list 
                              (list
                               (expr->string/C++ b tmp)
                               (sprintf "gsl_vector_set (f,~A,~A);" ni tmp))
                              (cons tmp lbs))
                             ))
                      ))
                  rate-eq-defs))
                ))

             (ss-vars (delete-duplicates
                       (map ->string 
                            (filter (lambda (x) (not (member x builtin-consts)))
                                    (append 
                                     init-locals
                                     init-order
                                     (map first external-eq-defs)
                                     (map pool-ion-in pool-ions)
                                     (map pool-ion-out pool-ions)
                                     (map first i-eqs)
                                     (map first steady-state-index-map) 
                                     (map first const-defs)
                                     (map first reaction-eq-defs)
                                     )))
                       string=?))
             

             (default-eqs 
               (map (lambda (def)
                      (let ((n (first def))
                            (b (second def)))
                        (expr->string/C++ (nest-name b) (nest-name n))))
                    defaults))

             (dynamics-eqs 
              (append 
               
               external-eq-defs
               asgn-eq-defs
               reaction-eq-defs
               
               (map (lambda (pool-ion)
                      (let ((n (pool-ion-in pool-ion))
                            (b (pool-ion-inq pool-ion)))
                        (list n b)))
                    pool-ions)
               
               (map (lambda (pool-ion)
                      (let ((n (pool-ion-out pool-ion))
                            (b (pool-ion-outq pool-ion)))
                        (list n b)))
                    pool-ions)
               
               i-eqs
               ))
             
             (dynamics-eq-dag 
              (map (lambda (def)
                     (cons (first def) (enum-freevars (second def) '() '())))
                   dynamics-eqs))

             (dynamics-eq-order
              (reverse
               (topological-sort dynamics-eq-dag 
                                 (lambda (x y) (string=? (->string x) (->string y))))))

             (dynamics-eq-locals  (find-locals 
                                   (map second
                                        (append i-eqs rate-eq-defs dynamics-eqs))))

             (dynamics-vars       (delete-duplicates 
                                   (map (compose ->string nest-name)
                                        (filter (lambda (x) 
                                                  (not (member x builtin-consts)))
                                                (append 
                                                 dynamics-eq-locals
                                                 dynamics-eq-order
                                                 (map first i-eqs)
                                                 (map first external-eq-defs)
                                                 (map first state-index-map)
                                                 (map first const-defs)
                                                 )))
                                   string=?))

             (residual-rate-eq-defs (map (lambda (def)
                                           (let* ((n (first def))
                                                  (i (lookup-def n state-index-map))
                                                  (fv (ith 'f i))
                                                  (y1v (ith 'y1 i))
                                                  (ypv (ith 'yp i))
                                                  )
                                             (expr->string/C++ `(- ,y1v ,ypv) fv)))
                                         rate-eq-defs))

             (define-fn  (make-define-fn sysname))
             
             (tmpl-env
              (fold tenv-enter '()
                    `(
                      (currentTimestamp  . ,(seconds->string (current-seconds)))
                      (nemoVersionString . ,(nemo:version-string))
                      (modelName         . ,sysname)

                      (abstol            . ,abstol)
                      (reltol            . ,reltol)
                      (ODEmethod         . ,method)
                      (SSmethod          . ,ss-method)
                      (SSvector          . ,(gensym 'ssvect))

                      (stateSize         . ,(length state-index-map))
                      (steadyStateSize   . ,(length steady-state-index-map))
                      (stateIndexMap     . ,((lambda (x) 
                                               (if (null? x) (ersatz:Tobj '()) x))
                                             (map (lambda (x) (cons (first x) (second x))) state-index-map)))
                      (steadyStateIndexMap . ,((lambda (x) 
                                                 (if (null? x) (ersatz:Tobj '()) x))
                                               (map (lambda (x) (cons (first x) (second x))) steady-state-index-map)))
                      (stateDefs           . ,(map (lambda (def)
                                                     (let* ((n      (first def)) 
                                                            (nu     (lookup-def n c-units))
                                                            (nscale (and nu (nemo:unit-scale nu))))
                                                       `(
                                                         (name  . ,(nest-name n))
                                                         (scale . ,nscale)
                                                         )
                                                       ))
                                                   state-index-map))
                      (defaultDefs         . ,(map first defaults))

                      (hasEvents         . ,(not (null? transient-event-defs)))
                      (exports           . ,(map nest-name exports))
                      (functionDefs      . ,(map (lambda (fndef) 
                                                   (and
                                                    (not (member (car fndef) builtin-fns))
                                                    (begin
                                                      (apply define-fn (cons indent fndef))
                                                      )))
                                               defuns))
                      (currentEqDefs     . ,(map
                                             (lambda (def) 
                                               (expr->string/C++ (second def) (first def)))
                                             i-eqs))
                      (residualRateEqDefs . ,residual-rate-eq-defs)
                      (defaultEqDefs      . ,default-eqs)
                      (constraintEqDefs   . 
                                          ,(map (lambda (eq)
                                                  (match-let 
                                                   (((op left right)  eq))
                                                   `((op . ,op)
                                                     (left . ,(expr->string/C++ (canonicalize-expr/C++ (rhsexpr/C++ left))))
                                                     (right . ,(expr->string/C++ (canonicalize-expr/C++ (rhsexpr/C++ right))))
                                                     (str . ,(->string eq))
                                                     )))
                                                c-eqs))

                      (synapticEventDefs
                       . 
                       ,(map 
                         (lambda (isyn psc transients)
                           
                           (let* (
                                  (ltransient-event-defs
                                   (filter (lambda (x) (member (first x) transients))
                                           transient-event-defs))
                                  
                                  (levent-external-eq-def
                                   (car
                                    (fold (lambda (def ax)
                                            (let* ((b (second def))
                                                   (events (let ((fvs (enum-freevars b '() '())))
                                                             (filter (lambda (x) (member (first x) fvs)) 
                                                                     event-external-eq-defs))))
                                              (append events ax)))
                                          '() ltransient-event-defs)))
                                  
                                  (lconsts
                                   (delete-duplicates
                                    (fold (lambda (def ax)
                                            (let* ((n (nest-name (first def)) )
                                                   (b (second def))
                                                   (consts (let ((fvs (enum-freevars b '() '())))
                                                             (filter (lambda (x) (member (first x) fvs)) const-defs)))
                                                   )
                                              (append consts ax)))
                                          '() ltransient-event-defs)
                                    (lambda (x y) (equal? (first x) (first y))))
                                   )
                                  
                                  (vars (append (map (compose nest-name car) lconsts)
                                                (list (nest-name (second levent-external-eq-def)))))
                                  )
                             `(
                               (pscName . ,(nest-name (first psc)))
                               (pscId . ,(nest-name (second psc)))
                               
                               (wscale . ,(let ((wscale (fourth isyn)))
                                            (and wscale (nest-name wscale))))

                               (wthreshold . ,(let ((wthreshold (fifth isyn)))
                                                (and wthreshold (nest-name wthreshold))))
                               
                               (eventVar . ,(second levent-external-eq-def))
                               
                               (eventVarEqDef . ,(let ((b (second levent-external-eq-def)))
                                                   (sprintf "~A = B_.spike_~A.get_value(lag);" 
                                                            b (nest-name (second psc)))))
                               
                               (externalEventEqDef . 
                                                   ,(let* ((n      (nest-name (first levent-external-eq-def)))
                                                           (nu     (lookup-def n c-units))
                                                           (nscale (and nu (nemo:unit-scale nu)))
                                                           (b      (second levent-external-eq-def))
                                                           )
                                                      (expr->string/C++ (if nscale `(* ,nscale ,b) b) n)))
                               
                               (transientEventEqDefs 
                                . 
                                ,(fold (lambda (def lst)
                                         (let* (
                                                (n  (nest-name (first def)) )
                                                (ni (lookup-def n state-index-map))
                                                (b  (second def))
                                                (consts (let ((fvs (enum-freevars b '() '())))
                                                          (filter (lambda (x) (member (first x) fvs)) 
                                                                  const-defs)))
                                                )
                                           (append
                                            (map (lambda (x) 
                                                   (let ((n (nest-name (first x))))
                                                     (sprintf "~A = P_.~A;" n n)))
                                                 consts)
                                            (if ni (list (expr->string/C++ (getstate ni) n)) '())
                                            (list (expr->string/C++ b n))
                                            (if ni (list (expr->string/C++ n (getstate ni))) '())
                                            lst
                                            )
                                           ))
                                       '()
                                       ltransient-event-defs))
                               
                               (localVars . ,(append (map (compose nest-name car) lconsts)
                                                     (list 
                                                      (nest-name (first levent-external-eq-def))
                                                      (nest-name (second levent-external-eq-def))
                                                      )
                                                     (map (lambda (x) (nest-name (first x))) ltransient-event-defs)
                                                     ))
                               ))
                           )
                         
                         i-syns pscs psc-transients))

                      (dynamics
                       .
                       (
                        (localVars     . ,dynamics-vars)
                        
                        (parameterDefs .
                                       ,(map (lambda (def)
                                               (let ((n (first def)) )
                                                 (expr->string/C++ (sprintf "params->~A" n) n)))
                                             const-defs))
                        
                        (ratePrevEqDefs . 
                                        ,(map (lambda (def)
                                                (let* ((n (first def))
                                                       (ni (lookup-def n state-index-map)))
                                                  (expr->string/C++ (ith 'y ni) (nest-name n))))
                                              rate-eq-defs))
                        
                        (eqOrderDefs . 
                                     ,(filter-map (lambda (n)
                                                    (let ((b (lookup-def n dynamics-eqs)))
                                                      (and b (expr->string/C++ b (nest-name n)))))
                                                  dynamics-eq-order))
                        
                        (rateEqDefs . 
                                    ,(map (lambda (def)
                                            (let* ((n (first def))
                                                   (b (second def))
                                                   (fv (ith 'f (lookup-def n state-index-map)))
                                                   )
                                              (expr->string/C++ b fv)))
                                          rate-eq-defs))
                        ))

                      (init
                       . 
                       (
                        
                        (localVars         . ,init-vars)
                        (parameterDefs     . 
                                           ,(map (lambda (def)
                                                   (let ((n (first def)) )
                                                     (expr->string/C++ (sprintf "p.~A" n) n)))
                                                 const-defs))
                        
                        (initOrder         . 
                                           ,(filter-map (lambda (n)
                                                          (let ((b  (lookup-def n init-eqs)))
                                                            (and b (expr->string/C++ b (nest-name n)))))
                                                        init-order))
                        
                        (initEqDefs        . 
                                           ,(filter-map (lambda (def)
                                                          (let* ((n  (first def)) 
                                                                 (ni (lookup-def n state-index-map)))
                                                            (and ni (expr->string/C++ n (sprintf "y_[~A]" ni)))))
                                                        init-eq-defs))
                        
                        (rateEqStates      . 
                                           ,(map first rate-eq-defs))
                        
                        (reactionEqDefs    . 
                                           ,(filter-map
                                             (lambda (def)
                                               (let ((n (first def)) (b (second def)))
                                                 (and (not (lookup-def n init-eq-defs))
                                                      (expr->string/C++ b n))))
                                             reaction-eq-defs))
                        
                        
                        ))
                      
                      (steadystate 
                       .
                       (
                        (localVars         . ,ss-vars)
                        
                        (parameterDefs     . ,(map
                                               (lambda (x) 
                                                 (let* ((n  (first x)))
                                                   (expr->string/C++ (sprintf "params->~A" n) n)))
                                               const-defs))
                        
                        (SScurrentEqDefs   . ,(map (lambda (def) 
                                                     (expr->string/C++ 0. (first def)))
                                                   i-eqs))
                        
                        (SSgetStateDefs    . ,ss-get-state-defs)
                        
                        (SSsetStateDefsLbs . ,(map (lambda (def+lbs) 
                                                     `((defs . ,(first def+lbs))
                                                       (lbs . ,(second def+lbs))))
                                                   ss-set-state-defs+lbs))
                        ))
                      
                      (parameters  
                       . 
                       (
                        (localVars       . ,(find-locals (map second const-defs)))
                        
                        (parameterEqDefs . ,(map (lambda (def)
                                                   (let* ((n  (first def)) (b (second def)))
                                                     (s+ (nest-name n) "  (" (expr->string/C++ b) ")")))
                                                 const-defs) )
                        
                        (parameterDefs  . ,(map (lambda (def)
                                                  (let* ((n      (first def)) 
                                                         (nu     (lookup-def n c-units))
                                                         (nscale (and nu (nemo:unit-scale nu))))
                                                    `(
                                                      (name  . ,(nest-name n))
                                                      (scale . ,nscale)
                                                      )
                                                    ))
                                                const-defs))
                        
                        (defaultDefs  . ,(map (lambda (def)
                                                (let* ((n      (first def)) 
                                                       (b      (second def))
                                                       (nu     (lookup-def n c-units))
                                                       (nscale (and nu (nemo:unit-scale nu))))
                                                  `(
                                                    (name  . ,(nest-name n))
                                                    (scale . ,nscale)
                                                    )
                                                  ))
                                              defaults))
                        
                        ))
                      ))
              ))

	(for-each 
	 (lambda (a)
	   (let ((acc-ion   (car a)))
	     (if (assoc acc-ion perm-ions)
		 (nemo:error 'nemo:nest-translator 
			     ": ion species " acc-ion " cannot be declared as both accumulating and permeating"))))
	 acc-ions)

        (if (not (= (length event-external-eq-defs) (length pscs)))
            (error 'nemo:nest-translator "mismatch between event variables and synaptic conductances" 
                   event-external-eq-defs pscs))

        (if dump-template-env
            (for-each (lambda (entry)
                        (fprintf (current-error-port)
                                 "~A = ~A~%" (car entry) 
                                 (ersatz:tvalue->pystr (cdr entry))))
                      tmpl-env
                      ))
        
	(let ((cpp-output  (open-output-file (make-output-fname dirname prefix ".cpp")))
	      (hpp-output  (open-output-file (make-output-fname dirname prefix ".h"))))
	  
	  (with-output-to-port cpp-output
	    (lambda () (instantiate-template nest-template tmpl-env) ))

          (close-output-port cpp-output)
	  
	  (with-output-to-port hpp-output
	    (lambda () (instantiate-template nest-header-template tmpl-env) ))
	  
          (close-output-port hpp-output)
             
          ))
      ))
  ))
  )

)

