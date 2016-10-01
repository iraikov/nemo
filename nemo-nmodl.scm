;;       
;; 
;; An extension for translating NEMO models to NMODL descriptions.
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

(module nemo-nmodl

	(nemo:nmodl-translator)

	(import scheme chicken utils data-structures srfi-1 srfi-13 srfi-69)

	(require-extension lolevel posix datatype matchable strictly-pretty 
			   varsubst datatype 
			   nemo-core nemo-utils nemo-gate-complex nemo-synapse)
	(require-library ersatz-lib)

        (import (prefix ersatz-lib ersatz: ))


(define (safe-car x)
  (and x (car x)))

(define (safe-cadr x)
  (and x (cadr x)))


(define nmodl-ops
  `(+ - * / > < <= >= = ^))


(define (nmodl-name s)
  (let ((cs (string->list (->string s))))
    (let loop ((lst (list)) (cs cs))
      (if (null? cs) (string->symbol (list->string (reverse lst)))
	  (let* ((c (car cs))
		 (c1 (cond ((or (char-alphabetic? c) (char-numeric? c) (char=? c #\_)) c)
			   (else #\_))))
	    (loop (cons c1 lst) (cdr cs)))))))
			    

(define (nmodl-state-name n s)
  (nmodl-name (if n (s+ n s) s)))

(define builtin-consts
  (append `(celsius diam)
          (map (lambda (x) (nmodl-name (first x))) nemo:math-constants)))

(define builtin-fns
  `(+ - * / pow neg abs atan asin acos sin cos exp ln
      sqrt tan cosh sinh tanh hypot gamma lgamma log10 log2 log1p ldexp cube
      > < <= >= = and or round ceiling floor max min
      ))

(define-datatype useion useion?
  (UseIon (name symbol?) (read list?) (write list?) (valence (lambda (x) (or (not x) (integer? x)))))
  (NonSpecific (name symbol?)))


(define (consolidate-useions useions)
  (let recur ((useions useions) (ax '()))
    (if (null? useions) ax
	(let ((u (car useions)))
	  (cases useion u
		 (UseIon (name read write valence)
			 (let ((v (lookup-def name ax)))
			   (if (not v) 
			       (recur (cdr useions) (cons (cons name u) ax))
			       (cases useion v 
				      (UseIon (_ read1 write1 valence1)
					 (if (and valence valence1 (not (equal? valence valence1)))
					     (error 'nemo-nmodl "ionic species has different declared valences" name valence valence1)
					     (recur (cdr useions)
						    (alist-update name (UseIon name
									       (delete-duplicates (append read read1) eq?)
									       (delete-duplicates (append write write1) eq?)
									       (or valence valence1)) ax))))
				      (NonSpecific (name)
						   (error 'nemo-nmodl "previously declared ionic species appears as non-specific" name)))
			       )))
		 (NonSpecific (name)
			      (recur (cdr useions) (cons (cons name u) ax)))
		 )
	  ))
    ))
				      
				      
			       

(define (rhsvars rhs)
  (enum-freevars rhs (list) (list)))


(define (rewrite-pow expr)
  (match expr
         (('pow x y)  (if (and (integer? y)  (positive? y))
			  (if (> y 1)  (let ((tmp (gensym "x")))
					 `(let ((,tmp ,x)) (* . ,(list-tabulate (inexact->exact y) (lambda (i) tmp)))))
			      x)
			  (if (and (number? y) (zero? y)) 1.0 expr)))
         (else expr)))


(define (rhsexpr/NMODL expr)
  (match expr 
	 (('if . es)  `(if . ,(map (lambda (x) (rhsexpr/NMODL x)) es)))
	 (('pow x y)  (cond ((and (integer? y) (= y 1)) x)
                            ((and (number? y) (zero? y)) 1.0)
                            (else expr)))
	 (('let bnds body) `(let ,(map (lambda (x) (list (car x) (rhsexpr/NMODL (cadr x)))) bnds) ,(rhsexpr/NMODL body)))
	 ((s . es)    (if (symbol? s)   (cons (if (member s builtin-fns) s (nmodl-name s)) 
					      (map (lambda (x) (rhsexpr/NMODL x)) es)) expr))
	 (id          (if (symbol? id) (nmodl-name id) id))))


(define-syntax pp
  (syntax-rules ()
    ((pp indent val ...) (ppf indent (quasiquote val) ...))))


(define (letblk/NMODL e1 e2)
  (cond ((equal? e1 (doc:empty)) (doc:group (doc:nest 2 e2)))
	((equal? e2 (doc:empty)) (doc:group (doc:nest 2 e1)))
	(else (doc:connect (doc:group (doc:nest 2 e1))
			   (doc:group (doc:nest 2 e2))))))
	
(define ifthen/NMODL  (doc:ifthen 0 (doc:text "if") (doc:text "") (doc:text "else")))
(define group/NMODL   (doc:block 2 (doc:text "(") (doc:text ")")))
(define block/NMODL   (doc:block 2 (doc:text "{") (doc:text "}")))
(define binop/NMODL   (doc:binop 2))

(define (format-op/NMODL indent op args)
  (let ((op1 (doc:text (->string op))))
    (let ((res
	   (if (null? args) op1
	       (match args
		      ((x)           (doc:connect op1 x))
		      ((x y)         (binop/NMODL x op1 y))
		      ((x y z)       (binop/NMODL x op1 (binop/NMODL y op1 z)))
		      (lst           (let* ((n   (length lst))
					    (n/2 (inexact->exact (round (/ n 2)))))
				       (binop/NMODL (format-op/NMODL indent op (take lst n/2 )) op1 
						    (format-op/NMODL indent op (drop lst n/2 )))))))))
      res)))


(define (format-conseq-op/NMODL indent op args)
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
			     (list (format-conseq-op/NMODL indent op (take lst n/2 )) op1 
				   (format-conseq-op/NMODL indent op (drop lst n/2 )))
			     (doc:space)))))))))

(define (format-fncall/NMODL indent op args)
  (let ((op1 (doc:text (->string op))))
    (doc:cons op1 (group/NMODL ((doc:list indent identity (lambda () (doc:text ", "))) args)))))

(define (name-normalize expr)
  (match expr 
	 (('if c t e)  `(if ,(name-normalize c) ,(name-normalize t) ,(name-normalize e)))
	 (('let bs e)
	  `(let ,(map (lambda (b) `(,(car b) ,(name-normalize (cadr b)))) bs) ,(name-normalize e)))
	 ((f . es) 
	  (cons (if (member f builtin-fns) f (nmodl-name f)) (map name-normalize es)))
	 ((? symbol? ) (nmodl-name expr))
	 ((? atom? ) expr)))


(define (canonicalize-expr/NMODL expr)
  (let ((subst-convert  (subst-driver (lambda (x) (and (symbol? x) x)) 
				      nemo:binding? identity nemo:bind 
				      nemo:subst-term)))
    (let* ((expr1 (if-convert expr))
	   (expr2 (subst-convert expr1 subst-empty))
	   (expr3 (let-lift expr2))
	   (expr4 (name-normalize expr3)))
      expr4)))


(define (format-expr/NMODL indent expr . rest)  
  (let-optionals rest ((rv #f))
   (let ((indent+ (+ 2 indent)))
    (match expr
       (('let bindings body)
	(letblk/NMODL
	 (fold-right 
	   (lambda (x ax)
	     (let ((res
		    (letblk/NMODL
		     (match (second x)
			    (('if c t e)
			     (ifthen/NMODL
			      (group/NMODL (format-expr/NMODL indent c))
			      (block/NMODL (format-expr/NMODL indent t (first x)))
			      (block/NMODL (format-expr/NMODL indent e (first x)))))
			    (else 
                             (doc:cons
                              (format-op/NMODL indent+ " = "
                                               (list (format-expr/NMODL indent (first x) )
                                                     (format-expr/NMODL indent (second x))))
                              (doc:text "\n"))))
                     ax)))
	       res
	       ))
	   (doc:empty) bindings)
	 (match body
		(('let _ _) (format-expr/NMODL indent body rv))
		(else
		 (let ((body1 (doc:nest indent (format-expr/NMODL indent body))))
		   (if rv  (format-op/NMODL indent " = " (list (format-expr/NMODL indent+ rv ) body1))
		       body1))))))
       
       (('if . rest) (error 'format-expr/NMODL "invalid if statement " expr))

       ((op . rest)  
       (let ((op (case op ((pow)  '^) ((abs) 'fabs) ((ln) 'log) (else op))))
	 (let ((fe
		(if (member op nmodl-ops)
		    (let ((mdiv?  (any (lambda (x) (match x (('* . _) #t) (('/ . _) #t) (else #f))) rest))
			  (mul?   (any (lambda (x) (match x (('* . _) #t) (else #f))) rest))
			  (plmin? (any (lambda (x) (match x (('+ . _) #t) (('- . _) #t) (else #f))) rest)))
		      (case op
			((/)  
			 (format-op/NMODL indent op 
					  (map (lambda (x) 
						 (let ((fx (format-expr/NMODL indent+ x)))
						   (if (or (symbol? x) (number? x)) fx
						       (if (or mul? plmin?) (group/NMODL fx) fx)))) rest)))
			((*)  
			 (format-op/NMODL indent op 
					  (map (lambda (x) 
						 (let ((fx (format-expr/NMODL indent+ x)))
						   (if (or (symbol? x) (number? x)) fx
						       (if plmin? (group/NMODL fx) fx)))) rest)))
			
			((^)  
			 (format-op/NMODL indent op 
					  (map (lambda (x) 
						 (let ((fx (format-expr/NMODL indent+ x)))
						   (if (or (symbol? x)  (number? x)) fx
						       (if (or mdiv? plmin?) (group/NMODL fx) fx)))) rest)))
			
			(else
			 (format-op/NMODL indent op 
					  (map (lambda (x) 
						 (let ((fx (format-expr/NMODL indent+ x))) fx)) rest)))))
		    
		    (let ((op (case op ((neg) '-) (else op))))
		      (format-fncall/NMODL indent op (map (lambda (x) (format-expr/NMODL indent+ x)) rest))))))
	   (if rv 
	       (format-op/NMODL indent " = " (list (format-expr/NMODL indent+ rv ) fe))
	       fe))))
      
      (else  (let ((fe (doc:text (->string expr))))
	       (if rv 
		   (format-op/NMODL indent " = " (list (format-expr/NMODL indent+ rv ) fe))
		   fe)))))))
	       

	  
(define (expr->string/NMODL x . rest)
  (let-optionals rest ((rv #f) (width 72))
    (sdoc->string (doc:format width (format-expr/NMODL 2 x rv)))))
  

(define (format-conseq/NMODL indent expr . rest)  
  (let-optionals rest ((rv #f))
   (let ((indent+ (+ 2 indent)))
    (match expr
       (('let bindings body)
	(letblk/NMODL
	 (fold-right 
	   (lambda (x ax)
	     (letblk/NMODL
	      (match (second x)
		     (('if c t e)
		      (ifthen/NMODL
		       (group/NMODL (format-conseq/NMODL indent c))
		       (block/NMODL (format-conseq/NMODL indent t (first x)))
		       (block/NMODL (format-conseq/NMODL indent e (first x)))))
		     (else 
		      (format-conseq-op/NMODL indent+ " = "
				       (list (format-conseq/NMODL indent (first x) )
					     (format-conseq/NMODL indent (second x))))))
	      ax))
	   (doc:empty) bindings)
	 (let ((body1 (doc:nest indent (format-conseq/NMODL indent body))))
	   (if rv  (format-conseq-op/NMODL indent " = " (list (format-conseq/NMODL indent+ rv ) body1))
	       body1))))
       
       (('if . rest) (error 'format-conseq/NMODL "invalid if statement " expr))

       ((op . rest)  
	(let ((op (case op ((pow)  '^) ((abs) 'fabs) (else op))))
	  (let ((fe
		(if (member op nmodl-ops)
		    (let ((mdiv?  (any (lambda (x) (match x (('* . _) #t) (('/ . _) #t) (else #f))) rest))
			  (mul?   (any (lambda (x) (match x (('* . _) #t) (else #f))) rest))
			  (plmin? (any (lambda (x) (match x (('+ . _) #t) (('- . _) #t) (else #f))) rest)))
		      (case op
			((/)  
			 (format-conseq-op/NMODL indent op 
					  (map (lambda (x) 
						 (let ((fx (format-conseq/NMODL indent+ x)))
						   (if (or (symbol? x) (number? x)) fx
						       (if (or mul? plmin?) (group/NMODL fx) fx)))) rest)))
			((*)  
			 (format-conseq-op/NMODL indent op 
					  (map (lambda (x) 
						 (let ((fx (format-conseq/NMODL indent+ x)))
						   (if (or (symbol? x) (number? x)) fx
						       (if plmin? (group/NMODL fx) fx)))) rest)))
			
			((^)  
			 (format-conseq-op/NMODL indent op 
					  (map (lambda (x) 
						 (let ((fx (format-conseq/NMODL indent+ x)))
						   (if (or (symbol? x)  (number? x)) fx
						       (if (or mdiv? plmin?) (group/NMODL fx) fx)))) rest)))
			
			(else
			 (format-conseq-op/NMODL indent op 
					  (map (lambda (x) 
						 (let ((fx (format-conseq/NMODL indent+ x))) fx)) rest)))))
		    
		    (case op
		      ((neg) (format-conseq-op/NMODL indent '* (map (lambda (x) (format-conseq/NMODL indent+ x)) 
							     (cons "(-1)" rest))))
		      (else  (format-fncall/NMODL indent op (map (lambda (x) (format-conseq/NMODL indent+ x)) 
								 rest)))))))

	   (if rv (format-conseq-op/NMODL indent " = " (list (format-conseq/NMODL indent+ rv ) fe)) fe))))
      
      (else  (let ((fe (doc:text (->string expr))))
	       (if rv 
		   (format-conseq-op/NMODL indent " = " (list (format-conseq/NMODL indent+ rv ) fe))
		   fe)))))))
	       

(define (reaction-keqs n initial open transitions power)
  (let* ((subst-convert  (subst-driver (lambda (x) (and (symbol? x) x)) 
				       nemo:binding? identity nemo:bind nemo:subst-term))
	 (state-list     (let loop ((lst (list)) (tlst transitions))
			   (if (null? tlst)  (delete-duplicates lst eq?)
			       (match (car tlst) 
				      (('-> (and (? symbol?) s0) (and (? symbol?) s1) rate-expr)
				       (loop (cons* s0 s1 lst) (cdr tlst)))
				      (((and (? symbol?) s0) '-> (and (? symbol? s1)) rate-expr)
				       (loop (cons* s0 s1 lst) (cdr tlst)))
				      (('<-> (and (? symbol?) s0) (and (? symbol?) s1) rate-expr1 rate-expr2)
				       (loop (cons* s0 s1 lst) (cdr tlst)))
				      (((and (? symbol?) s0) '-> (and (? symbol? s1)) rate-expr1 rate-expr2)
				       (loop (cons* s0 s1 lst) (cdr tlst)))
				      (else
				       (nemo:error 'nemo:nmodl-reaction-keqs: "invalid transition equation " 
						   (car tlst) " in state complex " n)))
                               )))
	 (state-subs     (fold (lambda (s ax) (subst-extend s (nmodl-state-name n s) ax)) subst-empty state-list)))
    ;; generate kinetic equations for each edge in the transitions system
    (list n 
	  (map
	   (lambda (e) 
	     (match e
		    (('-> s0 s1 rexpr)
		     (let ((i  (lookup-def s0 state-subs))
			   (j  (lookup-def s1 state-subs)))
		       `(-> ,i ,j ,(canonicalize-expr/NMODL 
				    (subst-convert rexpr state-subs)))))
		    
		    ((s0 '-> s1 rexpr)
		     (let ((i  (lookup-def s0 state-subs))
			   (j  (lookup-def s1 state-subs)))
		       `(-> ,i ,j ,(canonicalize-expr/NMODL 
				    (subst-convert rexpr state-subs)))))
		    
		    (('<-> s0 s1 rexpr1 rexpr2)
		     (let ((i  (lookup-def s0 state-subs))
			   (j  (lookup-def s1 state-subs)))
		       `(<-> ,i ,j 
			     ,(canonicalize-expr/NMODL (subst-convert rexpr1 state-subs))
			     ,(canonicalize-expr/NMODL (subst-convert rexpr2 state-subs)))))
		    
		    ((s0 '<-> s1 rexpr1 rexpr2)
		     (let ((i  (lookup-def s0 state-subs))
			   (j  (lookup-def s1 state-subs)))
		       `(<-> ,i ,j 
			     ,(canonicalize-expr/NMODL (subst-convert rexpr1 state-subs))
			     ,(canonicalize-expr/NMODL (subst-convert rexpr2 state-subs)))))
		    
		    
		    (else (nemo:error 'nemo:nmodl-reaction-keqs: "invalid transition equation " 
				      e " in state complex " n))))
	   transitions))))
	

(define (poset->kinetic-eq-defs poset sys kinetic)
  (fold-right
   (lambda (lst ax)
     (fold  (lambda (x ax) 
	      (match-let (((i . n)  x))
			 (let ((en (hash-table-ref sys n)))
			   (if (and (member n kinetic) (nemo:quantity? en))
			       (cases nemo:quantity en
				      (REACTION  (name initial open transitions conserve power u) 
						 (cons (reaction-keqs name initial open transitions power) ax))
				      (else  ax))
			       ax))))
	    ax lst))
   (list) poset))


(define (member-imports x imports)
  (safe-car (member x imports (lambda (x y) (equal? x (cadr y))))))



(define (conserve-conseq->string/NMODL x val . rest)
  (let-optionals rest ((width 72))
    (s+ "CONSERVE " (sdoc->string (doc:format width (format-conseq/NMODL 2 x #f))) 
	" = " (number->string val))))


(define (fndef->tmpl n proc)

  (let ((lst (procedure-data proc))
        (indent+ 2))
    
    (let ((rt       (lookup-def 'rt lst))
          (formals  (lookup-def 'formals lst))
          (vars     (lookup-def 'vars lst))
          (body     (lookup-def 'body lst)))
      
      (let* (
             (body0 (rhsexpr/NMODL body))
             (body1 (canonicalize-expr/NMODL body0))
             (lbs   (enum-bnds body1 (list)))
             (tmpl-vars
              `(
                (indent       . ,(ersatz:sexpr->tvalue indent+))
                (name         . ,(ersatz:sexpr->tvalue (nmodl-name n)))
                (vars         . ,(ersatz:sexpr->tvalue vars))
                (localVars    . ,(if (null? lbs) (ersatz:Tlist '()) (ersatz:sexpr->tvalue lbs)))
                (exprString   . ,(ersatz:Tstr (expr->string/NMODL body1 (nmodl-name n))))
              ))
             )

        tmpl-vars
        ))
    ))


(define nmodl-template
  (ersatz:statements-from-file 
   (ersatz:template-std-env 
    search-path: `(,template-dir))
   "NMODL.tmpl"))


(define (nemo:nmodl-translator sys . rest)

  (define (cid x)  (second x))
  (define (cn x)   (first x))

  (let-optionals rest ((method 'cnexp) (kinetic (list)) (dump-template-env #f))
  (match-let ((($ nemo:quantity 'DISPATCH  dis) (hash-table-ref sys (nemo-intern 'dispatch))))
    (let ((eval-const  (let ((eval-const (dis 'eval-const)))
                         (lambda (x q) (eval-const sys x q))))
          (imports     ((dis 'imports)  sys))
	  (exports     ((dis 'exports)  sys))
          )
      (let* (
             (indent        0)
	     (indent+       (+ 2 indent ))
	     (sysname       (nmodl-name ((dis 'sysname) sys)))
	     (consts        ((dis 'consts)  sys))
	     (asgns         ((dis 'asgns)   sys))
	     (states        ((dis 'states)  sys))
	     (kinetic       (or kinetic '()))
	     (kinetic       (delete-duplicates
			     (cond ((eq? kinetic 'all) (filter-map first states))
				   ((symbol? kinetic)  
				    (let ((sk (->string kinetic)))
				      (filter-map
                                       (lambda (s) (and s (and (string-suffix? sk (->string s)) s)) )
                                       (map first states))))
				   (else 
				    (let ((kinetic (map ->string kinetic))
					  (ss      (map first states)))
				      (concatenate
				       (map (lambda (sk)
					      (filter-map (lambda (s) (and (string-suffix? sk (->string s)) s))
							  ss))
					    kinetic)))))))
	     (reactions     ((dis 'reactions) sys))
	     (rates         ((dis 'rates) sys))
	     (defuns        ((dis 'defuns)  sys))
	     (components    ((dis 'components) sys))
	     (g             (match-let (((state-list asgn-list g) ((dis 'depgraph*) sys))) g))
	     (poset         (vector->list ((dis 'depgraph->bfs-dist-poset) g)))
	     
	     (gate-complex-info    (nemo:gate-complex-query sys))
	     (gate-complexes       (lookup-def 'gate-complexes gate-complex-info))
	     (perm-ions     (map (match-lambda ((comp i e erev val) 
						`(,comp ,(nmodl-name i) ,(nmodl-name e) ,erev ,val))
					       ((comp i e erev) 
						`(,comp ,(nmodl-name i) ,(nmodl-name e) ,erev #f)))
				 (lookup-def 'perm-ions gate-complex-info)))
	     (acc-ions      (delete-duplicates 
                             (map (match-lambda ((comp i in out)
                                                 `(,comp ,@(map nmodl-name (list i in out)))))
                                  (lookup-def 'acc-ions gate-complex-info))
                             (lambda (x y) (eq? (car x) (car y)))))
	     (mod-ions      (lookup-def 'mod-ions gate-complex-info))
	     (epools        (lookup-def 'pool-ions gate-complex-info))
	     (pool-ions     (pool-ion-name-map nmodl-name epools))

	     (i-gates       (lookup-def 'i-gates gate-complex-info))

	     (synapse-info  (nemo:post-synaptic-conductance-query sys))
             (isyns         (lookup-def 'i-synapses synapse-info))
             (pscs          (lookup-def 'post-synaptic-conductances synapse-info))


             (parameter-defs (filter-map
                              (lambda (nv)
                                (and (not (member (first nv) builtin-consts))
                                     (let ((v1 (canonicalize-expr/NMODL (second nv))))
                                       (list (first nv) v1))))
                              consts))

	     (external-eq-defs   (sys->external-eq-defs sys nmodl-name rhsexpr/NMODL canonicalize-expr/NMODL
                                                        namespace-filter: (lambda (x) (not (equal? x 'event)))))
             (external-event-eq-defs   (sys->external-eq-defs 
                                        sys nmodl-name rhsexpr/NMODL canonicalize-expr/NMODL
                                        namespace-filter: (lambda (x) (equal? x 'event))))

	     (asgn-eq-defs       (poset->asgn-eq-defs poset sys nmodl-name rhsexpr/NMODL canonicalize-expr/NMODL))
	     (asgn-eq-defs       (append asgn-eq-defs
					 (filter-map
					  (lambda (gate-complex) 
					    
					    (let* ((label             (first gate-complex))
						   (n                 (second gate-complex))
						   (subcomps          ((dis 'component-subcomps) sys n))
						   (permeability      (lookup-def 'permeability subcomps))
						   (permion           (lookup-def 'permeating-ion subcomps))
						   (permqs            (and permion ((dis 'component-exports) sys (cid permion))))
						   )
					      
					      (if (and permion (null? permqs) (not permeability))
						  (nemo:error 'nemo:nmodl-translator: "permeating-ion component in complex " label
							  " does not export any quantities; it must export reversal potential quantity"))

					      (if (and permion (not (null? permqs)))
						  
						  (case (cn permion)
						    ((non-specific)
						     (let* ((e      (nmodl-name 'e))
							    (elocal (car permqs)))
						       (and (not (equal? e elocal))
							   (list e (nmodl-name elocal)))))
						    
						    (else
						     (let* ((e (nmodl-name (s+ 'e (cn permion))))
							    (elocal (car permqs)))
						       (and (not (equal? e elocal))
							   (list e (nmodl-name elocal)))))
						    ) #f)
					      ))
					  gate-complexes)
                                         ))
	     (reaction-eq-defs   (poset->reaction-eq-defs poset sys nmodl-name nmodl-state-name rhsexpr/NMODL canonicalize-expr/NMODL)) 
	     (rate-eq-defs       (reverse (poset->rate-eq-defs poset sys method nmodl-name nmodl-state-name rhsexpr/NMODL canonicalize-expr/NMODL 
                                                               kinetic: kinetic)))
	     (kstate-eq-defs     (poset->kinetic-eq-defs poset sys kinetic))
	     (conserve-eq-defs   (poset->state-conserve-eq-defs poset sys nmodl-name nmodl-state-name))
	     (state-init-defs    (poset->init-defs poset sys nmodl-name nmodl-state-name rhsexpr/NMODL canonicalize-expr/NMODL))

	     (transient-event-defs  (poset->transient-event-defs poset sys method nmodl-name nmodl-state-name rhsexpr/NMODL canonicalize-expr/NMODL builtin-fns)) 

	     (useions   (append

                         ;; Synaptic currents are modeled as non-specific currents. 
                         (map (lambda (isyn) (NonSpecific (nmodl-name (first isyn)))) isyns)

                         ;; NEURON distinguishes between different
                         ;; calcium pools by calling their ions ca2,
                         ;; ca3, etc. This means that it is possible
                         ;; to have calcium buffer ca2 that imports
                         ;; ica to estimate the current calcium
                         ;; concentration. So we check if the current
                         ;; model is importing currents from different
                         ;; species and generate useion statements to
                         ;; cover this case.
                         
                         (filter-map (lambda (x) (and (equal? (third x) 'ion-currents) 
                                                      (let* ((n (nmodl-name (second x)))
                                                             (ion (nmodl-name (string-drop (->string n) 1))))
                                                        (UseIon ion (list n) (list) #f ))))
                                     imports)
                         
                         ;; For every current defined in the current
                         ;; model, we must generate useion statements
                         ;; for the current and optionally reversal
                         ;; potential and ionic concentrations
			 (filter-map (lambda (x)
				       (case (first x)

					 ((non-specific) 
					  (NonSpecific (second x)))

					 (else 
                                          (cond
                                           ((fourth x) ;; there is erev present, i.e. ohmic channel
					       (let* ((ion    (first x))

						      ;; this handles a special case when a mechanism is defined as 
						      ;; ohmic (i.e. with reversal potential), but uses dynamic ionic 
						      ;; concentrations to compute reversal potential; 
						      ;; in this case, the reversal potential is computed 
						      ;; in the mechanism at each timestep and we must not do USEION READ e... 

						      (concqs  (filter identity
								       (list (safe-cadr (member-imports (string->symbol (s+ ion 'i)) imports))
									     (safe-cadr (member-imports (string->symbol (s+ ion 'o)) imports)))))

						      ;; if ionic concentrations are not imported in this mechanism, 
						      ;; just use the reversal potential set for this ionic species

						      (readqs (if (null? concqs) (list (third x)) concqs))

						      (writeqs (list (second x)))

						      (valence (and (fifth x) (inexact->exact (eval-const (fifth x) (fifth x)))))
                                                      )

						 (UseIon ion readqs writeqs valence)))
                                            
                                           (else ;; no erev present, probably a concentration-based channel
                                            (let* ((ion    (first x))

                                                   (concqs  (filter identity
                                                                    (list (safe-cadr (member-imports (string->symbol (s+ ion 'i)) imports))
                                                                          (safe-cadr (member-imports (string->symbol (s+ ion 'o)) imports)))))
                                                   
                                                   (readqs concqs)
                                                   
                                                   (writeqs (list (second x)))
                                                   
                                                   (valence (and (fifth x) (inexact->exact (eval-const (fifth x) (fifth x)))))
                                                   )

						 (UseIon ion readqs writeqs valence)))
					 ))
                                       ))
				     (delete-duplicates perm-ions (lambda (x y) (eq? (car x) (car y)) ))
				     )
			 
			 (if (null? acc-ions)

			     (append 

			      (map (lambda (pool-ion)
				     (let ((valence (pool-ion-valence pool-ion)))
				       (UseIon (pool-ion-name pool-ion) 
					       (list (pool-ion-cur pool-ion))
					       (list (pool-ion-in pool-ion ) (pool-ion-out pool-ion ))
					       (and valence (inexact->exact (eval-const valence valence)))
					       )))
				   pool-ions)

			      (map (lambda (mod-ion)
				     (let ((valence (fourth mod-ion)))
				       (let ((read-ions (filter (lambda (x) 
								  (and x (member-imports x imports)))
								(list (second mod-ion) (third mod-ion))))
					     (valence (and valence (inexact->exact (eval-const valence valence)))))
				       (if (null? read-ions)
					   (let ((ion-name (car mod-ion)))
					     (nemo:error 'nemo:nmodl-translator: "there are no imported quantities for modulating ion " ion-name
							 "; the model must import " (s+ ion-name 'i) " or " (s+ ion-name 'o) " or both")))
				       (UseIon (first mod-ion) read-ions (list) valence))))
				   mod-ions))

			     (map (lambda (acc-ion)
				    (let ((pool-ion (assoc (first acc-ion) (map (lambda (p) (cons (pool-ion-name p) p)) pool-ions))))
				      (if pool-ion
					  (let ((pool-ion (cdr pool-ion)))
					    (UseIon (first acc-ion) 
						    (list (third acc-ion) (fourth acc-ion) (pool-ion-in pool-ion))
						    (list (second acc-ion) (pool-ion-out pool-ion ))
						    #f))
					  (UseIon (first acc-ion) 
						  (list (third acc-ion) (fourth acc-ion))
						  (list (second acc-ion))
						  #f))))
				  acc-ions)
			 )))

	     (useions   (consolidate-useions useions))


             (i-eqs (filter-map
		       (lambda (gate-complex) 
			 
			 (let* ((label             (first gate-complex))
				(n                 (second gate-complex))
				(subcomps          ((dis 'component-subcomps) sys n))
				(acc               (lookup-def 'accumulating-substance subcomps))
				(perm              (lookup-def 'permeating-ion subcomps))
				(permqs            (and perm ((dis 'component-exports) sys (cid perm))))
				(pore              (lookup-def 'pore subcomps))
				(permeability      (lookup-def 'permeability subcomps))
				(gates             (filter (lambda (x) (equal? (car x) 'gate)) subcomps))
				(sts               (map (lambda (gate) ((dis 'component-exports) sys (cid gate))) gates)))
			   

			   (if (and pore (null? permqs))
			       (nemo:error 'nemo:nmodl-translator: "ion channel definition " label
                                              "permeating-ion component lacks exported quantities"))
			   
			   (for-each 
			    (lambda (st)
			      (if (null? st)
				  (nemo:error 'nemo:nmodl-translator: "ion channel definition " label
					      "gate component lacks exported quantities")))
			    sts)
			   
			   (if (not (or pore permeability))
			       (nemo:error 'nemo:nmodl-translator: "ion channel definition " label
					      "lacks any pore or permeability components"))
			   
			   (cond ((and perm permeability (pair? gates))
				  (let* ((i     (nmodl-name (s+ 'i (cn perm))))
					 (pmax  (car ((dis 'component-exports) sys (cid permeability))))
					 (pwrs  (map (lambda (st) (map (lambda (n) (state-power sys n)) st)) sts))
					 (gpwrs (map (lambda (st pwr) (map (lambda (s p) (if p `(pow ,s ,p) s)) st pwr)) sts pwrs))
					 (gion  `(* ,pmax ,(sum (map (lambda (gpwr) 
								       (match gpwr ((x)  x) (else `(* ,@gpwr))))
								     gpwrs))))
					 )
				    (list i #f gion (nmodl-name (s+ 'i_ label) ))))
				 
				 ((and perm pore (pair? gates))
				  
				  (case (cn perm)
				    ((non-specific)
				     (let* ((i      (nmodl-name 'i))
					    (e      (car permqs))
					    (gmax   (car ((dis 'component-exports) sys (cid pore))))
					    (pwrs   (map (lambda (st) (map (lambda (n) (state-power sys n)) st)) sts))
					    (gpwrs  (map (lambda (st pwr) (map (lambda (s p) (if p `(pow ,s ,p) s)) st pwr)) sts pwrs))
					    (gion   `(* ,gmax ,(sum (map (lambda (gpwr) 
									  (match gpwr ((x)  x) (else `(* ,@gpwr))))
									gpwrs))))
					    )
				       (list i e gion (nmodl-name (s+ 'i_ label) ))))
				    
				    (else
				     (let* ((i          (nmodl-name (s+ 'i (cn perm))))
					    (e          (car permqs))
					    (gmax       (car ((dis 'component-exports) sys (cid pore))))
					    (pwrs  (map (lambda (st) (map (lambda (n) (state-power sys n)) st)) sts))
					    (gpwrs (map (lambda (st pwr) (map (lambda (s p) (if p `(pow ,s ,p) s)) st pwr)) sts pwrs))
					    (gion  `(* ,gmax ,(sum (map (lambda (gpwr) 
									  (match gpwr ((x)  x) (else `(* ,@gpwr))))
									gpwrs))))
					    )
				       (list i e gion (nmodl-name (s+ 'i_ label)))))))
				 
				 ((and perm pore)
				  (case (cn perm)
				    ((non-specific)
				     (let* ((i      (nmodl-name 'i))
					    (e      (car permqs))
					    (gmax   (car ((dis 'component-exports) sys (cid pore)))))
				       (list i e gmax (nmodl-name (s+ 'i_ label)))))
				    
				    (else
				     (nemo:error 'nemo:nmodl-translator: "ion channel definition " label
						 (s+ "(" n ")")
						 "lacks gate component"))))
				 
				 ((and acc pore (pair? gates))
				  (let* ((i     (nmodl-name (s+ 'i (cn acc))))
					 (gmax  (car ((dis 'component-exports) sys (cid pore))))
					 (pwrs  (map (lambda (st) (map (lambda (n) (state-power sys n)) st)) sts))
					 (gpwrs (map (lambda (st pwr) (map (lambda (s p) (if p `(pow ,s ,p) s)) st pwr)) sts pwrs))
					 (gion  `(* ,gmax ,(sum (map (lambda (gpwr) 
								       (match gpwr ((x)  x) (else `(* ,@gpwr))))
								     gpwrs))))
					 )
				    (list i #f gion (nmodl-name (s+ 'i_ label) ))))
				 
				 (else (nemo:error 'nemo:nmodl-translator: "invalid ion channel definition " 
						   label))
				 )))
		       gate-complexes))

	       (i-eqs  (fold  (lambda (i-gate ax) 
				(let ((i-gate-var (first i-gate)))
				  (cons (list (nmodl-name 'i) #f i-gate-var (s+ 'i_ (second i-gate)) ) ax)))
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
						    (sum1  (rhsexpr/NMODL sum0))
						    (sum2  (canonicalize-expr/NMODL sum1)))
					       (append eqs (list (list i sum2)) ax))
					     
					     (match-let (((i e gion ii) (car ps)))
							
							(loop (cdr ps) 
							      (cons ii summands) 
							      (let* ((expr0 (rhsexpr/NMODL (if e `(* ,gion (- v ,e)) gion)))
								     (expr1 (canonicalize-expr/NMODL expr0)))
								(cons (list ii expr1) eqs)))))))
				      
				      ((i e gion ii)
				       (let* ((expr0  (rhsexpr/NMODL (if e `(* ,gion (- v ,e)) gion)))
					      (expr1  (canonicalize-expr/NMODL expr0)))
					 (cons (list i expr1) ax)))
				      
				      (else ax)))
				(list) i-bkts))
	       
	       (current-locals (find-locals (map second i-eqs)))

	       (parameter-locals  (find-locals (map second parameter-defs)))
	       
	       (state-defs 
		(append
		 (map (lambda (st)
			(if (pair? st) (nmodl-state-name (first st) (second st)) 
			    (nmodl-name st)))
		      states)
		 (map nmodl-name reactions)))
	       
	       (assigned-defs
		(filter-map
		 (lambda (x) 
		   (let ((x1 (nmodl-name x)))
		     (and (not (or (member x1 state-defs) (assoc x1 parameter-defs)))
			  x1)))
		 (delete-duplicates
		  (append asgns
			  (filter-map first imports)
			  (filter-map (lambda (x) (and (not (equal? (third x) 'event)) (second x))) imports)
			  (map second perm-ions) (map third perm-ions)
			  (map second acc-ions)  (map fourth acc-ions)
			  (map pool-ion-in pool-ions) (map pool-ion-out pool-ions)
			  (map (lambda (gate-complex) (nmodl-name (s+ 'i_ (first gate-complex)))) gate-complexes )
			  (map (lambda (i-gate) (nmodl-name (s+ 'i_ (second i-gate)))) i-gates )
			  (map (lambda (isyn) (nmodl-name (first isyn))) isyns )
			  (map (lambda (psc) (nmodl-name (s+ 'i_ (first psc)))) pscs )
			  ))
                 ))

               (asgn-locals (find-locals (map second asgn-eq-defs)))

               (reaction-locals (find-locals (map second reaction-eq-defs)))

               (rate-locals  (find-locals (map second rate-eq-defs)))

               (init-locals (find-locals (map second state-init-defs)))

               (event-locals (find-locals (map second transient-event-defs)))
               (event-vars   (delete-duplicates
                              (filter-map 
                               (lambda (x) (let ((ns (third x)))
                                             (and (equal? ns 'event) (second x))))
                               imports)))


               )
	     
        (let* (

               (prime (lambda (x) (s+ x "'")))

               (tmpl-env
                (fold tenv-enter '()
                `(
                  (indent            . ,indent+)
                  (ODEmethod         . ,method)
                  (modelName         . ,sysname)
                  (currentTimestamp  . ,(seconds->string (current-seconds)))
                  (nemoVersionString . ,(nemo:version-string))
                  (hasEvents         . ,(not (null? transient-event-defs)))
                  (exports           . ,(map nmodl-name exports))
                  (currents          . ,(append 
                                         (map (lambda (gate-complex) (nmodl-name (s+ 'i_ (first gate-complex)))) gate-complexes )
                                         (map (lambda (i-gate) (nmodl-name (s+ 'i_ (second i-gate)))) i-gates )))
                  (currentEqLocals   . ,current-locals)
                  (currentEqDefs     . ,(map (lambda (p) (expr->string/NMODL (second p) (first p))) i-eqs))
                  (permeatingIons    . ,(map
                                         (match-lambda
                                          ((comp i e erev val) 
                                           `((species . ,comp) (i . ,(nmodl-name i)) 
                                             (e . ,(nmodl-name e)) (erev . ,erev) (valence . ,val)))
                                          ((comp i e erev) 
                                           `((species . ,comp) (i . ,(nmodl-name i)) 
                                             (e . ,(nmodl-name e)) (erev . ,erev))))
                                         perm-ions))
                  (modulatingIons     . ,(filter-map
                                          (match-lambda 
                                           ((ion in-conc out-conc val)
                                            (let ((qs (filter (lambda (x) (and x (member-imports x imports)) )
                                                              (list in-conc out-conc))))
                                              (and (not (null? qs))
                                                   `((in . ,in-conc) (out . ,out-conc))))))
                                           mod-ions))
                  (accumulatingIons   . ,(map
                                          (match-lambda ((comp i in out)
                                                         `((species . ,comp) (i . ,(nmodl-name i))
                                                           (in . ,(nmodl-name in)) (out . ,(nmodl-name out)))))
                                          acc-ions))

                  (poolIons           . ,(map
                                          (lambda (pool-ion)
                                            `((ion      . ,(pool-ion-name pool-ion))
                                              (inq      . ,(pool-ion-inq pool-ion))
                                              (outq     . ,(pool-ion-outq pool-ion))
                                              (cur      . ,(pool-ion-cur pool-ion))
                                              (in       . ,(pool-ion-in pool-ion))
                                              (out      . ,(pool-ion-out pool-ion))
                                              (valence  . ,(pool-ion-valence pool-ion))))
                                          pool-ions))
                  
                  (useIons            . ,(map
                                          (lambda (x) 
                                            (let ((u (cdr x)))
                                              (cases useion u
                                                     (UseIon (name read write valence)
                                                             `((nonSpecific . #f)
                                                               (name . ,name)
                                                               (read . ,read)
                                                               (write . ,write)
                                                               (valence . ,valence)))
                                                     (NonSpecific (name)
                                                                  `((nonSpecific . #t)
                                                                    (name . ,name))))
                                              ))
                                          useions))

                  (rangeParameters    .  ,(let* (
                                                 (param-names  (map (compose nmodl-name first) parameter-defs))
                                                 (is-const?    (lambda (x) (member x param-names)))
                                                 )
                                           (delete-duplicates 
                                            (fold (lambda (def ax) 
                                                    (let* ((rhs   (second def))
                                                           (vars  (cond ((nemo:rhs? rhs)
                                                                         (rhsvars rhs))
                                                                        ((extended-procedure? rhs)
                                                                         (let* ((fd  (procedure-data rhs))
                                                                                (cs  (lookup-def 'consts fd)))
                                                                           (map (compose nmodl-name first) cs)
                                                                           ))
                                                                        (else '())
                                                                        ))
                                                           )
                                                      (append (filter is-const? vars) ax)))
                                                  (list) 
                                                  (append asgn-eq-defs rate-eq-defs reaction-eq-defs defuns )))))
                  
                  (parameterLocals . ,parameter-locals)

                  (parameterDefs . ,(map (lambda (def)
                                           (let ((n (nmodl-name (first def))) (b (second def)))
                                             (expr->string/NMODL b n)))
                                         parameter-defs))

                  (stateDefs . ,state-defs )

                  (assignedDefs . ,assigned-defs)

                  (assignedEqLocals . ,asgn-locals)

                  (assignedEqDefs . ,(map (lambda (def)
                                            (let ((n (nmodl-name (first def))) (b (second def)))
                                              (expr->string/NMODL b n)))
                                          asgn-eq-defs))

                  (reactionEqLocals . ,reaction-locals)

                  (reactionEqDefs . ,(map (lambda (def)
                                            (let ((n (nmodl-name (first def))) (b (second def)))
                                              (expr->string/NMODL b n)))
                                          reaction-eq-defs))

                  (rateEqLocals . ,rate-locals) 

                  (rateEqDefs . ,(map (lambda (def)
                                        (let ((n (prime (first def)))
                                              (b (second def)))
                                          (expr->string/NMODL b n)))
                                      rate-eq-defs))

                  (externalEqDefs . ,(map (lambda (def)
                                            (let ((n (nmodl-name (first def))) (b (second def)))
                                              (expr->string/NMODL b n)))
                                          external-eq-defs))

                  (kineticEqLocals . ,(let* ((exprs (concatenate (map second kstate-eq-defs))))
                                        (concatenate 
                                         (find-locals
                                          (append (map fourth exprs)
                                                  (filter-map (lambda (x) (and (> (length x) 4) (fifth x))) exprs))))))

                  (kineticEqDefs . ,(map 
                                     (lambda (def)
                                       (let* ((n (first def))
                                              (eqs (let ((eqs (second def)))
                                                     (let-values (((pair-eqs1 rest)
                                                                   (partition (lambda (eq) (match eq (('<-> a b r1 r2) eq) (else #f))) eqs)))
                                                       (let ((pair-eqs2
                                                              (append
                                                               (filter-map 
                                                                (lambda (eq)
                                                                  (match eq
                                                                         (('-> s0 s1 rexpr) 
                                                                          (let ((rev
                                                                                 (find (lambda (r) 
                                                                                         (match r (('-> t0 t1 texpr)
                                                                                                   (and (equal? s0 t1) (equal? s1 t0) texpr))
                                                                                                (else #f)))
                                                                                       eqs)))
                                                                            (if rev
                                                                                `(<-> ,s0 ,s1 ,rexpr ,(fourth rev))
                                                                                (error  'nemo-nmodl "-> kinetic equation not supported in NMODL" eq))))
                                                                         (('<-> s0 s1 rexpr1 rexpr2)  #f)
                                                                         ))
                                                                rest))))
                                                         (append pair-eqs1 (delete-duplicates 
                                                                            pair-eqs2 
                                                                            (lambda (x y) 
                                                                              (match (list x y)
                                                                                     ((('<-> s0 s1 s t) ('<-> t0 t1 u v))
                                                                                      (and (equal? s0 t1) (equal? s1 t0)))
                                                                                     (else #f)))))
                                                         
                                                         ))
                                                     ))
                                              (conserve-eqs  (lookup-def (nmodl-name n) conserve-eq-defs)))

                                         (append
                                          (map
                                           (lambda (eq)
                                             (match eq
                                                    (('-> s0 s1 rexpr) 
                                                     (error 'nemo-nmodl "-> kinetic equation not supported in NMODL" eq))
                                                    (('<-> s0 s1 rexpr1 rexpr2)  
                                                     (sprintf "~~ ~A <-> ~A (~A, ~A)"
                                                              s0 s1
                                                              (expr->string/NMODL rexpr1) 
                                                              (expr->string/NMODL rexpr2) 
                                                              ))
                                                    ))
                                           eqs)
                                          (map (lambda (eq) 
                                                 (let ((val  (first eq))
                                                       (expr (third eq)))
                                                   (conserve-conseq->string/NMODL expr val)))
                                               conserve-eqs))
                                         ))
                                     kstate-eq-defs))


                  (reversalPotentialEqDefs .
                                           ,(filter-map
                                             (lambda (gate-complex) 
                                               (let* ((label             (first gate-complex))
                                                      (n                 (second gate-complex))
                                                      (subcomps          ((dis 'component-subcomps) sys n))
                                                      (perm              (lookup-def 'permeating-ion subcomps))
                                                      (permqs            (and perm ((dis 'component-exports) sys (cid perm))))
                                                      )
                                                 (and perm
                                                      (case (cn perm)
                                                        ((non-specific)
                                                         (let* ((e      (nmodl-name 'e))
                                                                (elocal (car permqs)))
                                                           (and (not (equal? e elocal))
                                                                (expr->string/NMODL (nmodl-name elocal) e))))
                                                        (else #f)))
                                                 ))
                                             gate-complexes))
                  

                  (initEqLocals . ,init-locals)

                  (initEqDefs . ,(map (lambda (def)
                                        (let ((n (first def)) (b (second def)))
                                          (expr->string/NMODL b n)))
                                      state-init-defs))

                  (eventLocals         . ,event-locals)
                  (eventVars           . ,event-vars)

                  (externalEventEqDefs . ,(map (lambda (def)
                                                 (let ((n (nmodl-name (first def)) )
                                                       (b (second def)))
                                                   (expr->string/NMODL b n)))
                                               external-event-eq-defs))

                  (transientEventEqDefs . ,(map (lambda (def)
                                                  (let ((n (nmodl-name (first def)) )
                                                        (b (second def)))
                                                    (expr->string/NMODL b n)))
                                                
                                                transient-event-defs))
                  (functionDefs . ,(map (lambda (fndef) 
                                          (if (not (member (car fndef) builtin-fns))
                                              (apply fndef->tmpl fndef)))
                                        defuns))

                  ))
                ))

          (if dump-template-env
              (for-each (lambda (entry)
                          (fprintf (current-error-port)
                                   "~A = ~A~%" (car entry) 
                                   (ersatz:tvalue->pystr (cdr entry))))
                        tmpl-env
                        ))

          (instantiate-template nmodl-template tmpl-env)
	  
	  ))
      ))
  ))
)


