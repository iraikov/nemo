;;       
;; 
;; An extension for generating Matlab/Octave code from NEMO models.
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

(module nemo-matlab

	(nemo:matlab-translator
	 nemo:octave-translator
         matlab-name)

	(import scheme chicken utils data-structures ports srfi-1 srfi-13 srfi-69)
	
	(require-extension lolevel posix matchable strictly-pretty 
			   varsubst datatype nemo-core nemo-utils
                           nemo-geometry nemo-defaults
			   nemo-gate-complex nemo-synapse nemo-currents)

(define matlab-ops
  `(+ - * / > < <= >= = ^))

(define (matlab-name s)
  (let ((cs (string->list (->string s))))
    (let loop ((lst (list)) (cs cs))
      (if (null? cs) (string->symbol (list->string (reverse lst)))
	  (let* ((c (car cs))
		 (c1 (cond ((or (char-alphabetic? c) (char-numeric? c) (char=? c #\_)) c)
			   (else #\_))))
	    (loop (cons c1 lst) (cdr cs)))))))

(define builtin-consts
  (map (lambda (x) (matlab-name (first x))) nemo:math-constants))

(define builtin-fns
  `(+ - * / pow neg abs atan asin acos sin cos exp ln
      sqrt tan cosh sinh tanh hypot gamma lgamma log10 log2 log1p ldexp cube
      > < <= >= = and or round ceiling floor max min struct
      ))

(define (matlab-state-name n s)
  (matlab-name (s+ n s)))
	    
(define (rhsexpr/MATLAB expr)
  (match expr 
	 (('if . es)  `(if . ,(map (lambda (x) (rhsexpr/MATLAB x)) es)))
	 (('pow x y)  (if (and (integer? y)  (positive? y))
			  (if (> y 1)  (let ((tmp (gensym "x")))
					 `(let ((,tmp ,x)) (* . ,(list-tabulate (inexact->exact y) (lambda (i) tmp)))))
			      x)
			    (if (and (number? y) (zero? y)) 1.0 expr)))
	 ((s . es)    (if (symbol? s)  (cons (if (member s builtin-fns) s (matlab-name s)) (map (lambda (x) (rhsexpr/MATLAB x)) es)) expr))
	 (id          (if (symbol? id) (matlab-name id) id))))

(define-syntax pp
  (syntax-rules ()
    ((pp indent val ...) (ppf indent (quasiquote val) ...))))


(define group/MATLAB   (doc:block 2 (doc:text "(") (doc:text ")")))
(define block/MATLAB   (doc:block 2 (doc:empty) (doc:empty)))
(define (stmt/MATLAB x) 
  (match x
	 (($ doc 'DocCons _ ($ doc 'DocText ";")) x)
	 (else  (doc:cons x (doc:text ";")))))


(define (ifthen/MATLAB c e1 e2)
  (doc:nest 
   2 (doc:connect
      (doc:connect (doc:group (doc:connect (doc:text "if") c))
		   (doc:connect (doc:nest 2 e1)
				(doc:nest 2 (doc:connect (doc:text "else") e2))))
      (doc:text "end"))))

(define (letblk/MATLAB e1 e2)
  (cond ((equal? e1 (doc:empty)) (doc:group (doc:nest 2 e2)))
	((equal? e2 (doc:empty)) (doc:group (doc:nest 2 e1)))
	(else (doc:connect (doc:group (doc:nest 2 (stmt/MATLAB e1)))
			   (doc:group (doc:nest 2 e2))))))
	

(define (format-op/MATLAB indent op args)
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
			     (list (format-op/MATLAB indent op (take lst n/2 )) op1 
				   (format-op/MATLAB indent op (drop lst n/2 )))
			     (doc:space)))))))))


(define (format-fncall/MATLAB indent op args)
  (let ((op1 (doc:text (->string op))))
    (doc:cons op1 (group/MATLAB ((doc:list indent identity (lambda () (doc:text ", "))) args)))))


(define (name-normalize expr)
  (match expr 
	 (('if c t e)  `(if ,(name-normalize c) ,(name-normalize t) ,(name-normalize e)))
	 (('let bs e)
	  `(let ,(map (lambda (b) `(,(car b) ,(name-normalize (cadr b)))) bs) ,(name-normalize e)))
	 ((f . es) 
	  (cons (if (member f builtin-fns) f (matlab-name f)) (map name-normalize es)))
	 ((? symbol? ) (matlab-name expr))
	 ((? atom? ) expr)))


(define (canonicalize-expr/MATLAB expr)
  (let ((subst-convert  (subst-driver (lambda (x) (and (symbol? x) x)) nemo:binding? identity nemo:bind nemo:subst-term)))
    (let* ((expr1 (if-convert expr))
	   (expr2 (subst-convert expr1 subst-empty))
	   (expr3 (let-lift expr2))
	   (expr4 (name-normalize expr3)))
      expr4)))


(define (format-expr/MATLAB indent expr . rest)  
  (let-optionals rest ((rv #f))
   (let ((indent+ (+ 2 indent)))
    (match expr
       (('let bindings body)
	(letblk/MATLAB
	 (fold-right 
	   (lambda (x ax)
	     (letblk/MATLAB
	      (match (second x)
		     (('if c t e)
		      (ifthen/MATLAB
		       (group/MATLAB (format-expr/MATLAB indent c))
		       (block/MATLAB (format-expr/MATLAB indent t (first x)))
		       (block/MATLAB (format-expr/MATLAB indent e (first x)))))
		     (else 
		      (stmt/MATLAB
		       (format-op/MATLAB indent+ " = "
					 (list (format-expr/MATLAB indent (first x) )
					       (format-expr/MATLAB indent (second x)))))))
	      ax))
	   (doc:empty) bindings)
	 (match body
		(('let _ _) (format-expr/MATLAB indent body rv))
		(else
		 (let ((body1 (doc:nest indent (format-expr/MATLAB indent body))))
		   (if rv (stmt/MATLAB (format-op/MATLAB indent " = " (list (format-expr/MATLAB indent+ rv ) body1)))
		       body1))))))
       
       (('if . rest) (error 'format-expr/MATLAB "invalid if statement " expr))

       ((op . rest)  
        (let ((op (case op ((pow)  '^)  ((ln) 'log) (else op))))
          (let ((fe
		(if (member op matlab-ops)
		    (let ((mdiv?  (any (lambda (x) (match x (('* . _) #t) (('/ . _) #t) (else #f))) rest))
			  (mul?   (any (lambda (x) (match x (('* . _) #t) (else #f))) rest))
			  (plmin? (any (lambda (x) (match x (('+ . _) #t) (('- . _) #t) (else #f))) rest)))
		      (case op
			((/)  
			 (format-op/MATLAB indent op 
					  (map (lambda (x) 
						 (let ((fx (format-expr/MATLAB indent+ x)))
						   (if (or (symbol? x) (number? x)) fx
						       (if (or mul? plmin?) (group/MATLAB fx) fx)))) rest)))
			((*)  
			 (format-op/MATLAB indent op 
					  (map (lambda (x) 
						 (let ((fx (format-expr/MATLAB indent+ x)))
						   (if (or (symbol? x) (number? x)) fx
						       (if plmin? (group/MATLAB fx) fx)))) rest)))
			
			((^)  
			 (format-op/MATLAB indent op 
					  (map (lambda (x) 
						 (let ((fx (format-expr/MATLAB indent+ x)))
						   (if (or (symbol? x)  (number? x)) fx
						       (if (or mdiv? plmin?) (group/MATLAB fx) fx)))) rest)))
			
			(else
			 (format-op/MATLAB indent op 
					  (map (lambda (x) 
						 (let ((fx (format-expr/MATLAB indent+ x))) fx)) rest)))))
		    
		    (let ((op (case op ((neg) '-) (else op))))
		      (format-fncall/MATLAB indent op (map (lambda (x) (format-expr/MATLAB indent+ x)) rest))))))
	   (if rv 
	       (stmt/MATLAB (format-op/MATLAB indent " = " (list (format-expr/MATLAB indent+ rv ) fe)))
	       fe))))
      
      (else  (let ((fe (doc:text (->string (if (symbol? expr) (matlab-name expr) expr)))))
	       (if rv 
		   (stmt/MATLAB (format-op/MATLAB indent " = " (list (format-expr/MATLAB indent+ rv ) fe)))
		   fe)))))))
	       
	  
(define (expr->string/MATLAB x . rest)
  (let-optionals rest ((rv #f) (width 72))
    (sdoc->string (doc:format width (format-expr/MATLAB 2 x rv)))))


  
(define (make-define-fn defuns)
  (lambda (indent n proc)
    (let ((lst (procedure-data proc))
	  (indent+ (+ 2 indent)))
      (let ((retval   (matlab-name (gensym "retval")))
	    (rt       (lookup-def 'rt lst))
	    (formals  (lookup-def 'formals lst))
	    (vars     (lookup-def 'vars lst))
            (consts   (filter (lambda (x) (not (procedure? (cdr x)))) (lookup-def 'consts lst)))
	    (body     (lookup-def 'body lst)))
	(pp indent ,nl (function ,retval = ,(matlab-name n) 
                                 (,(slp ", " (append vars '(params))))))
	(let* ((body0 (rhsexpr/MATLAB body))
	       (body1 (add-params-to-fncall (canonicalize-expr/MATLAB body0) 
                                            builtin-fns))
	       (lbs   (enum-bnds body1 (list))))
          (if (not (null? consts))
              (for-each (lambda (x) (let ((n (car x))) 
                                      (pp indent+ (,(matlab-name n) = ,(s+ "params." (matlab-name n) ";"))))) 
                        consts))
	  (pp indent+ ,(expr->string/MATLAB body1 retval))
	  (pp indent end))
	  ))
    ))



(define (output-jac sysname state-index-map pool-ions i-eqs v-eq unresolved-imports indent indent+)

  ;; Diagonal Jacobian approximation: (f(s+.01) - f(s))/.001 
  
  (if (null? unresolved-imports)
      (begin
        (pp indent ,nl (function res = ,(s+ sysname "_jac") (,(slp ", " `(vt vy vdy params)))))
        (pp indent+ 
            (dfdy = ,sysname (,(slp ", " `(vt "vy+0.01" params))) #\;)
            ))
      (begin
        (pp indent ,nl 
            (function res = ,(s+ sysname "_jac") (,(slp ", " `(vt vy vdy params imports)))))
        (pp indent+ 
            (dfdy = ,sysname (,(slp ", " `(vt "vy+0.01" params imports))) #\;)
            ))
      )

  (pp indent+ ("res = (dfdy - vdy) / 0.001;"))
  (pp indent "endfunction")

  )




(define (output-dy sysname state-index-map const-defs
                   rate-eq-defs reaction-eq-defs asgn-eq-defs external-eq-defs
                   pool-ions i-eqs v-eq unresolved-imports indent indent+)

  (let ((ret (if (pair? i-eqs) (s+ "[dy " (sw+ (map  car i-eqs)) "]") "dy")))

    (if (null? unresolved-imports)
        (pp indent ,nl (function ,ret = ,sysname (,(slp ", " `(t y params)))))
        (pp indent ,nl (function ,ret = ,sysname (,(slp ", " `(t y params imports)))))
        )
  
  (let ((vi (lookup-def 'v state-index-map)))
    (if vi (pp indent+ ,(expr->string/MATLAB (s+ "y(" vi ")") 'v))))
	   
  (for-each (lambda (def)
	      (let* ((n (first def)) 
		     (ni (lookup-def n state-index-map))
		     (yi (s+ "y" ni)))
		(pp indent+
		    ,(s+ "% rate eq " n)
		    ,(expr->string/MATLAB (s+ "y(" ni  ")") n))
		))
	    rate-eq-defs)
	   

  (for-each
   (lambda (def)
     (let ((n (matlab-name (first def))))
       (pp indent+ ,(expr->string/MATLAB (s+ "params." n) n))))
   const-defs)

  (let* ((eqs 
	  (append 

           (map
            (lambda (def)
              (let ((n (matlab-name (second def))))
                (list n (s+ "imports." n))))
            unresolved-imports)

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
	 
	 (eq-dag 
	  (map (lambda (def)
		 (cons (first def) (enum-freevars (second def) '() '())))
	       eqs))

	 (eq-order
	  (reverse
	   (topological-sort eq-dag 
			     (lambda (x y) (string=? (->string x) (->string y)))))))
	  (for-each (lambda (n)
		      (let ((b  (lookup-def n eqs)))

			(if b (pp indent+ 
				  ,(s+ "% equation for " n)
				  ,(expr->string/MATLAB b (matlab-name n))))))
		    eq-order))

  
  
  (pp indent+ ,(expr->string/MATLAB `(zeros (length y) 1) 'dy))
  
  (for-each (lambda (def)
	      (let ((n (s+ "dy(" (lookup-def (first def) state-index-map) ")")) 
		    (b (second def)))
		(pp indent+ ,(s+ "% state " (first def))
		    ,(expr->string/MATLAB b n))))
	    rate-eq-defs)

  (if v-eq
      (let ((vi (lookup-def 'v state-index-map)))
	(if vi (pp indent+ ,(expr->string/MATLAB (second v-eq) (s+ "dy(" vi ")"))))))

  (pp indent ,nl (end))
))


(define (output-steadystate  sysname steady-state-index-map pool-ions i-eqs
			     external-eq-defs const-defs init-eq-defs asgn-eq-defs 
                             rate-eq-defs reaction-eq-defs 
                             unresolved-imports  indent indent+)
  
  (if (not (null? steady-state-index-map))
      (begin
        (if (null? unresolved-imports)
            (pp indent ,nl (function y = ,(s+ sysname "_steadystate") (,(slp ", " '(x v params)))))
            (pp indent ,nl (function y = ,(s+ sysname "_steadystate") (,(slp ", " '(x v params imports))))))
            
	(for-each 
	 (lambda (def)
	   (let* ((n   (first def)) 
		  (ni  (lookup-def n steady-state-index-map)))
	     (if ni (pp indent+ ,(expr->string/MATLAB (s+ "x(" ni ")") n)))
	     ))
	 rate-eq-defs)
	
	(pp indent+ ,(expr->string/MATLAB `(zeros ,(length steady-state-index-map) 1) 'y))

	(let* ((init-eqs 
		(append 

                 (map
                  (lambda (def)
                    (let ((n (matlab-name (second def))))
                      (list n (s+ "imports." n))))
                  unresolved-imports)
		 
                 external-eq-defs
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
               )

          (for-each (lambda (def) 
                      (pp indent+ ,(expr->string/MATLAB 0. (first def))))
                    i-eqs)
          
          (for-each
           (lambda (def)
             (let ((n (matlab-name (first def))))
               (pp indent+ ,(expr->string/MATLAB (s+ "params." n) n))))
           const-defs)
          
	  (for-each (lambda (n)
		      (let ((b  (lookup-def n init-eqs)))
			(if b (pp indent+ ,(expr->string/MATLAB b (matlab-name n))))))
		    init-order))
	
	(for-each 
	 (lambda (def)
	   (let* ((n   (first def)) 
		  (ni  (lookup-def n steady-state-index-map))
		  (b   (second def)))
	     (if ni (pp indent+ ,(s+ "% state " n)
			,(expr->string/MATLAB b (s+ "y(" ni ")"))))
	     ))
	 rate-eq-defs)
	
	(pp indent ,nl (end))))
)


(define (output-print-state sysname state-index-map indent indent+)
  (pp indent ,nl (function  ,(s+ sysname "_print_state") (y)))
  
  (let ((lst (sort (map (lambda (x) (cons (->string (car x)) (cdr x))) state-index-map)
		   (lambda (x y) (string<? (car x) (car y))))))
    (for-each (lambda (p)
		(let ((n (first p)) (i (second p)))
		  (pp indent+ (,n " = " "y(" ,i ")"))))
	      lst))
  
  (pp indent ,nl (end))
)


(define (output-parameters sysname const-defs defaults indent indent+)
  (pp indent ,(expr->string/MATLAB 
               `(struct .
                 ,(fold-right 
                   (lambda (def ax)
                     (let ((n (matlab-name (first def)))
                           (b (second def)))
                       (cons (s+ #\" n #\")
                             (cons (expr->string/MATLAB b) ax))
                       ))
                   '()
                    const-defs))
               (s+ sysname "_parameters")))
  )


(define (output-init sysname state-index-map steady-state-index-map 
		     external-eq-defs const-defs asgn-eq-defs init-eq-defs 
                     rate-eq-defs reaction-eq-defs 
                     i-eqs pool-ions defaults unresolved-imports
                     indent indent+)

  (if (null? unresolved-imports)
      (pp indent ,nl (function y0 = ,(s+ sysname "_init") (,(slp ", " '(Vinit params)))))
      (pp indent ,nl (function y0 = ,(s+ sysname "_init") (,(slp ", " '(Vinit params imports)))))
      )
  
  (pp indent+ ,(expr->string/MATLAB `(zeros ,(length state-index-map) 1) 'y0))
  
  
  (let* ((init-eqs 
          (append 

           (map
            (lambda (def)
              (let ((n (matlab-name (second def))))
                (list n (s+ "imports." n))))
            unresolved-imports)

           (map (lambda (def)
                  (let ((n (matlab-name (first def)))
                        (b (matlab-name (second def))))
                    (list n b)))
                defaults)
           
           (map (lambda (def)
                  (let ((n (matlab-name (first def)))
                        (b (matlab-name (second def))))
                    (list n b)))
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
         )

    (let ((vi (lookup-def 'v state-index-map)))
      (pp indent+ ,(expr->string/MATLAB `Vinit 'v))
      (if vi (pp indent+ ,(expr->string/MATLAB 'v (s+ "y0(" vi ")") ))))

    (for-each (lambda (def) 
                (pp indent+ ,(expr->string/MATLAB 0. (first def))))
              i-eqs)

    (for-each
     (lambda (def)
       (let ((n (matlab-name (first def))))
         (pp indent+ ,(expr->string/MATLAB (s+ "params." n) n))))
     const-defs)
    
    (for-each (lambda (n)
                (let ((b  (lookup-def n init-eqs)))
                  (if b (pp indent+ ,(expr->string/MATLAB b (matlab-name n))))))
              init-order)
    )
  
  (for-each (lambda (def)
              (let* ((n  (first def)) 
                     (ni (lookup-def n state-index-map)))
                (if ni (pp indent+ ,(expr->string/MATLAB n  (s+ "y0(" ni ")"))))))
            init-eq-defs)
  
  (if (not (null? steady-state-index-map))
      (begin
        (pp indent+
            ,(expr->string/MATLAB `(zeros ,(length steady-state-index-map) 1) 'xs)
            ,(expr->string/MATLAB 
              (if (null? unresolved-imports)
                  `(fsolve ,(s+ "@(x) " sysname "_steadystate(x, v, params)") xs) 
                  `(fsolve ,(s+ "@(x) " sysname "_steadystate(x, v, params, imports)") xs) )
              "[ys,fval,info]"))
        
        
        (for-each
         (lambda (def)
           (let* ((n (first def)) 
                  (si (lookup-def n steady-state-index-map))
                  (ni (lookup-def n state-index-map)))
             (if si (begin 
                      (pp indent+ ,(expr->string/MATLAB (s+ "ys(" si ")") n))
                      (pp indent+ ,(expr->string/MATLAB (s+ "ys(" si ")") (s+ "y0(" ni ")")))))))
         rate-eq-defs)
        
        ))
  
  (for-each (lambda (def)
              (let ((n (first def)) (b (second def)))
                (if (not (lookup-def n init-eq-defs))
                    (pp indent+ ,(expr->string/MATLAB b n)))))
	    reaction-eq-defs)
  
  (for-each (lambda (def) 
              (pp indent+ ,(expr->string/MATLAB (second def) (first def))))
            i-eqs)
  
  (pp indent ,nl (end))
  
  )

(define (matlab-translator1 sys . rest)
  (define (cid x)  (second x))
  (define (cn x)   (first x))
  (let-optionals rest ((mode 'multiple) (filename #t) (dirname "."))
  (match-let ((($ nemo:quantity 'DISPATCH  dis) (hash-table-ref sys (nemo-intern 'dispatch))))
    (let ((imports  ((dis 'imports)  sys))
	  (exports  ((dis 'exports)  sys))
          (defaults (nemo:defaults-query sys))
          (geometry (nemo:geometry-query sys)))
      (let* ((indent      0)
	     (indent+     (+ 2 indent ))
	     (sysname     (matlab-name ((dis 'sysname) sys)))
	     (prefix      (->string sysname))
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
				  (and (not (member (first nv) builtin-consts))
				       (let ((v1 (canonicalize-expr/MATLAB (second nv))))
					 (list (matlab-name (first nv)) v1))))
				consts))
	     
             (gate-complex-info    (nemo:gate-complex-query sys))
	     (perm-ions       (map (match-lambda ((comp i e erev val) `(,comp ,(matlab-name i) ,(matlab-name e) ,erev)))
				   (lookup-def 'perm-ions gate-complex-info)))
	     (acc-ions        (map (match-lambda ((comp i in out) `(,comp ,@(map matlab-name (list i in out)))))
				   (lookup-def 'acc-ions gate-complex-info)))
	     (epools          (lookup-def 'pool-ions gate-complex-info))
	     (pool-ions       (pool-ion-name-map matlab-name epools))

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

	     (i-syns         (lookup-def 'i-synapses synapse-info))
		
	     (i-gates        (lookup-def 'i-gates gate-complex-info))

	     (i-defs         (nemo:ionic-current-definitions
                              gate-complexes i-gates i-syns pscs marea
                              (lambda (x) (state-power sys x))
                              (lambda (x) ((dis 'component-exports) sys x))
                              (lambda (x) ((dis 'component-subcomps) sys x))
                              matlab-name rhsexpr/MATLAB canonicalize-expr/MATLAB
                              builtin-fns))
             
             (i-eqs          (lookup-def 'i-eqs i-defs))
             (i-names        (lookup-def 'i-names i-defs))

	     (external-eq-defs   (sys->external-eq-defs sys matlab-name rhsexpr/MATLAB canonicalize-expr/MATLAB ))

	     (asgn-eq-defs       (poset->asgn-eq-defs* poset sys matlab-name rhsexpr/MATLAB canonicalize-expr/MATLAB builtin-fns ))
	     
	     (rate-eq-defs       (reverse (poset->rate-eq-defs* poset sys 'default matlab-name matlab-state-name 
                                                                rhsexpr/MATLAB canonicalize-expr/MATLAB builtin-fns )))

	     (reaction-eq-defs   (poset->reaction-eq-defs* poset sys matlab-name matlab-state-name rhsexpr/MATLAB canonicalize-expr/MATLAB ))
	     
	     (init-eq-defs       (poset->init-defs* poset sys matlab-name matlab-state-name rhsexpr/MATLAB canonicalize-expr/MATLAB builtin-fns ))
	     
	     (conserve-eq-defs   (map (lambda (eq) (list 0 `(- ,(second eq) ,(first eq)))) 
				      (poset->state-conserve-eq-defs poset sys matlab-name matlab-state-name )))
	     
	     (v-eq               (and (not (null? i-names))
                                      (cond
                                       ((and mrc marea)
                                        (list 'v (rhsexpr/MATLAB 
                                                  `(* 1e3 (/ (neg ,(sum i-names)) ,mrc)))))
                                       
                                       (marea
                                        (list 'v (rhsexpr/MATLAB 
                                                  `(* 1e3 (neg ,(sum i-names))))))
                                       
                                       (mrc
                                        (list 'v (rhsexpr/MATLAB `(* 1e3 (/ (neg ,(sum i-names)) ,mrc)))))
                                       
                                       (else
                                        (list 'v (rhsexpr/MATLAB `(* 1e3 (neg ,(sum i-names))))))
                                       ))
                                 )

	     
	     (state-index-map  (let ((acc (fold (lambda (def ax)
						  (let ((st-name (first def)))
						    (list (+ 1 (first ax)) 
							  (cons `(,st-name ,(first ax)) (second ax)))))
						(list 1 (list)) 
                                                (cons (list 'v) rate-eq-defs)
						)))
				 (second acc)))
	     
	     (steady-state-index-map  (let ((acc (fold (lambda (def ax)
							 (let ((st-name (first def)))
							   (if (not (alist-ref st-name init-eq-defs))
							       (list (+ 1 (first ax)) 
								     (cons `(,st-name ,(first ax)) (second ax)))
							       ax)))
						       (list 1 (list)) 
						       rate-eq-defs)))
					(second acc)))
	     
	     (dfenv 
	      (map (lambda (x) (let ((n (first x)))
				 (list n (matlab-name (s+ "d_" n )))))
		   defuns))

             (unresolved-imports (nemo:resolve-imports defaults imports exports epools))

	     )
	
	
	(for-each 
	 (lambda (a)
	   (let ((acc-ion   (car a)))
	     (if (assoc acc-ion perm-ions)
		 (nemo:error 'nemo:matlab-translator 
			     ": ion species " acc-ion " cannot be declared as both accumulating and permeating"))))
	 acc-ions)
	
	
	(let* ((output  (case mode
                          ((single)  (open-output-file (make-output-fname dirname prefix ".m" filename)))
                          (else #f)))
               
               (generated-by0
                (lambda ()
                  (if output 
                      (pp indent ,nl ("%{")
                          (This file was generated by ,(nemo:version-string) on ,(seconds->string (current-seconds)) )
                          ("%}") ))
                  ))
               
               (generated-by (if output (lambda () (begin)) generated-by0))
               )

          (if output (with-output-to-port output generated-by0))

          ;; derivative function
          (let ((output1 (or output (open-output-file (make-output-fname dirname prefix "_dy.m")))))
            (with-output-to-port output1
              (lambda ()
                (generated-by)
                (output-dy sysname state-index-map const-defs
                           rate-eq-defs reaction-eq-defs asgn-eq-defs external-eq-defs
                           pool-ions i-eqs v-eq unresolved-imports
                           indent indent+)))
            (if (not output) (close-output-port output1)))
          

          ;; Jacobian function
          (let ((output1 (or output (open-output-file (make-output-fname dirname prefix "_jac.m")))))
            (with-output-to-port output1
              (lambda ()
                (generated-by)
                (output-jac sysname state-index-map pool-ions i-eqs v-eq unresolved-imports indent indent+)))
              (if (not output) (close-output-port output1)))
          
          ;; steady state solver
          (let ((output1 (or output (open-output-file (make-output-fname dirname prefix "_steadystate.m")))))
            (with-output-to-port output1
              (lambda ()
                (generated-by)
                (output-steadystate sysname steady-state-index-map pool-ions i-eqs
                                    external-eq-defs const-defs init-eq-defs asgn-eq-defs 
                                    rate-eq-defs reaction-eq-defs 
                                    unresolved-imports indent indent+)))
            (if (not output) (close-output-port output1)))

          ;; initial values function
          (let ((output1 (or output (open-output-file (make-output-fname dirname prefix "_init.m")))))
            (with-output-to-port output1
              (lambda ()
                (generated-by)
                (output-init sysname state-index-map steady-state-index-map 
                             external-eq-defs const-defs asgn-eq-defs init-eq-defs 
                             rate-eq-defs reaction-eq-defs 
                             i-eqs pool-ions defaults unresolved-imports
                             indent indent+)
                (pp indent ,nl)))
            (if (not output) (close-output-port output1)))

          ;; default parameters
          (let ((output1 (or output (open-output-file (make-output-fname dirname prefix "_parameters.m")))))
            (with-output-to-port output1
              (lambda ()
                (generated-by)
                (output-parameters sysname const-defs defaults indent indent+)
                (pp indent ,nl)))
            (if (not output) (close-output-port output1)))
          
          
          ;; user-defined functions
          (let* (;;(with       (inexact->exact (round (/ (abs (- max-v min-v)) step))))
                 (define-fn  (make-define-fn defuns)))
            (for-each (lambda (fndef) 
                        (if (not (member (car fndef) builtin-fns))
                            (let ((output1 (or output (open-output-file (make-output-fname dirname (matlab-name (car fndef)) ".m")))))
                              (with-output-to-port output1
                                (lambda ()
                                  (generated-by)
                                  (apply define-fn (cons indent fndef))
                                  (pp indent ,nl)))
                              (if (not output) (close-output-port output1)))))
                      defuns))
          
          (if output (close-output-port output)))
        
        ))
    ))
  )


(define (nemo:matlab-translator sys . rest)
  (apply matlab-translator1 (cons sys (cons 'multiple rest))))

(define (nemo:octave-translator sys . rest)
  (apply matlab-translator1 (cons sys (cons 'single rest))))


)
