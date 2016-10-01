;;
;; NEMO substrate semantics.
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
;; TODO: 
;; * check that the various component types export the right number of quantities
;; * check that open states in reactions are valid
;; * check that reactions do not specify more than 
;;   one transition between any two states
;;

(module nemo-core

 (make-nemo-core nemo:error nemo:warning nemo:version-string
  nemo:env-copy nemo:quantity?
  nemo:rhs? nemo:expr? nemo:conseq? nemo:subst-term nemo:binding? nemo:bind
  nemo:math-constants
  nemo-intern nemo-scoped eval-nemo-system-decls 
  CONST ASGN REACTION RATE EXTERNAL PRIM LABEL)

 (import scheme chicken data-structures ports lolevel extras
	 srfi-1 srfi-4 srfi-13 srfi-14 srfi-69)
 
 (require-extension lolevel datatype matchable vector-lib
		    varsubst digraph graph-bfs graph-cycles
                    mathh nemo-units)


 (include "mathh-constants")


;--------------------
;  Message routines
;
;

(define (nemo:warning x . rest)
  (let loop ((port (open-output-string)) (objs (cons x rest)))
    (if (null? objs)
	(begin
	  (newline port)
	  (print-error-message (get-output-string port) 
			       (current-error-port) "nemo warning"))
	(begin (display (car objs) port)
	       (display " " port)
	       (loop port (cdr objs))))))


(define (nemo:error x . rest)
  (let ((port (open-output-string)))
    (if (port? x)
	(begin
	  (display "[" port)
	  (display (port-name x) port)
	  (display "] " port)))
    (let loop ((objs (if (port? x) rest (cons x rest))))
      (if (null? objs)
	  (begin
	    (newline port)
	    (error 'nemo (get-output-string port)))
	  (let ((obj (car objs)))
	    (if (procedure? obj) 
		(with-output-to-port port obj)
		(begin
		  (display obj port)
		  (display " " port)))
	    (loop (cdr objs)))))))


(include "nemo-version.scm")

(define (nemo:version-string) 
  (sprintf "NEMO (http://wiki.call-cc.org/nemo) version ~A~%" nemo-version))


(define (make-opt pred?) (lambda (x) 
			   (or (not x) (pred? x))))

(define (eval-math x . rest)
  (if (null? rest)
      (let ((ex `(begin (import mathh) ,x)))
        (eval ex))
      (let ((ex `(begin (import mathh) (list ,x . ,rest))))
        (eval ex))
      ))
      

(define (expr? x)  
  (or (symbol? x) (number? x) 
      (match x (((? symbol?) . rest)  (every expr? rest)) 
	     (((and hd (? expr?)) . rest)  (every expr? rest)) 
	     (else #f))))

(define (rhs? x)  
  (or (symbol? x) (number? x) 
      (match x 
	     (('let bnds body) (and (rhs? body)
				    (every (lambda (b) 
					     (and (symbol? (car b)) (rhs? (cadr b)))) bnds)))
	     (((? symbol?) . rest)  (every rhs? rest)) 
	     (else #f))))

(define (conseq? x)  (match x (((? number?) '= ('+ . rest))  #t) (else #f)))

(define nemo:expr?   expr?)
(define nemo:rhs?    rhs?)
(define nemo:conseq? conseq?)

(define (transition? x)
  (match x
	 (('-> a b r)       (and (symbol? a) (symbol? b) (rhs? r)))
	 ((a '-> b r)       (and (symbol? a) (symbol? b) (rhs? r)))
	 (('<-> a b r1 r2)  (and (symbol? a) (symbol? b) (rhs? r1) (rhs? r2)))
	 ((a '<-> b r1 r2)  (and (symbol? a) (symbol? b) (rhs? r1) (rhs? r2)))
	 (else #f)))

(define-datatype nemo:quantity nemo:quantity?
  (SYSNAME    (name symbol?))
  (LABEL      (v symbol?))
  (CONST      (name symbol?) (value number?) (u nemo:unit?))
  (ASGN       (name symbol?) (value number?) (rhs rhs?) (u nemo:unit?))
  (REACTION   (name symbol?) 
	      (initial      (lambda (x) (or (rhs? x) (not x))))
	      (open         (lambda (x) (or (symbol? x) (and (list? x) (every symbol? x) ))))
	      (transitions  (lambda (x) (and (list? x) (every transition? x)))) 
	      (conserve     (lambda (x) (or (not x) (and (list? x) (every conseq? x)))))
	      (power        integer?)
              (u            nemo:unit?)
              )
  (RATE       (name symbol?) 
	      (initial      (lambda (x) (or (rhs? x) (not x))))
	      (rhs          rhs?)
	      (power       (lambda (x) (or (integer? x) (not x))))
	      (u           nemo:unit?)
              )
  (TRANSIENT  (name symbol?) 
              (initial      (lambda (x) (or (rhs? x) (not x))))
              (rhs          rhs?)
              (asgn         rhs?)
              (power       (lambda (x) (or (integer? x) (not x))))
	      (u           nemo:unit?)
              )
  (PRIM       (name symbol?) (value identity))
  (EXTERNAL   (local-name symbol?) (name symbol?) (namespace (make-opt symbol?)) (u nemo:unit?))
  (EXPORTS    (lst (lambda (x) (and (list? x) (every symbol? x)))))
  (COMPONENT  (name symbol?) (type symbol?) (lst (lambda (x) (and (list? x) (every symbol? x)))) (scope-subst list?))
  (FUNCTOR    (name symbol?) (args (lambda (x) (and (list? x) (every symbol? x)))) (type symbol?)  (decls list?))
  (DISPATCH   (value procedure?))
  )

(define (nemo-intern sym)
  (string->symbol (string-append "#" (symbol->string sym))))

(define (nemo-scoped scope sym)
  (let ((ss (->string scope)))
    (if (string-null? ss) 
        (->string sym)
        (string->symbol (string-append ss ":" (->string sym))))
    ))

(define fresh (compose string->symbol symbol->string gensym))

(define (alist? x)
  (every (lambda (x) (and (pair? x) (symbol? (car x)))) x))

(define (lookup-def k lst . rest)
  (let-optionals rest ((default #f))
    (let ((k (->string k))) 
     (let recur ((kv #f) (lst lst))
       (if (or kv (null? lst))
	(if (not kv) default
 	    (match kv ((k v) v) (else (cdr kv))))
        (let ((kv (car lst)))
          (recur (and (string=? (->string (car kv)) k) kv)
		 (cdr lst)) ))))))


(define (nemo:subst-term t subst k)
  (assert (every symbol? (map car subst)))
  (if (null? subst) t
      (match t
	     (('if c t e)
	      `(if ,(k c subst) ,(k t subst) ,(k e subst)))
	     
	     (('let bs e)
	      (let ((r `(let ,(map (lambda (b) `(,(car b) ,(k (cadr b) subst))) bs) ,(k e subst))))
		(k r subst)))
	     
	     ((f . es)
	      (cons (k f subst) (map (lambda (e) (k e subst)) es)))
	     
	     ((? symbol? )  (lookup-def t subst t))
	     
	     ((? atom? ) t))
      ))
    

(define (nemo:binding? t) 
  (and (list? t) (eq? 'let (car t)) (cdr t)))

(define (nemo:bind ks vs e) `(let ,(zip ks vs) ,e))

(define nemo:env-copy hash-table-copy)

(define nemo:math-constants
  (zip 
   `(E 1/E E^2 E^PI/4 LOG2E LOG10E LN2 LN3 LNPI LN10 1/LN2 1/LN10 PI PI/2
       PI/4 1/PI 2/PI 2/SQRTPI SQRTPI PI^2 DEGREE SQRT2 1/SQRT2 SQRT3 SQRT5
       SQRT10 CUBERT2 CUBERT3 4THRT2 GAMMA1/2 GAMMA1/3 GAMMA2/3 PHI LNPHI
       1/LNPHI EULER E^EULER SIN1 COS1 ZETA3)
   (list E 1/E E^2 E^PI/4 LOG2E LOG10E LN2 LN3 LNPI LN10 1/LN2 1/LN10 PI PI/2
         PI/4 1/PI 2/PI 2/SQRTPI SQRTPI PI^2 DEGREE SQRT2 1/SQRT2 SQRT3 SQRT5
         SQRT10 CUBERT2 CUBERT3 4THRT2 GAMMA1/2 GAMMA1/3 GAMMA2/3 PHI LNPHI
         1/LNPHI EULER E^EULER SIN1 COS1 ZETA3)))
     


(define (make-nemo-core . alst)

  ;; floating point precision (single or double; default is double)
  (define  fptype (lookup-def 'fpprec alst 'double))
  (define  fpvector-type
    (lookup-def 'fpvector-type alst (if (equal? fptype 'single) 'f32vector 'f64vector)))
  (define  fpvector? 
    (lookup-def 'fpvector? alst (if (equal? fptype 'single) f32vector? f64vector?)))
  (define  fpvector  
    (lookup-def 'fpvector alst (if (equal? fptype 'single) f32vector f64vector)))
  (define  fpvector-ref
    (lookup-def 'fpvector-ref alst (if (equal? fptype 'single) f32vector-ref f64vector-ref)))
  (define  fpvector-set!
    (lookup-def 'fpvector-set! alst (if (equal? fptype 'single) f32vector-set! f64vector-set!)))


  (define builtin-fns
    `(+ - * / pow neg abs atan asin acos sin cos exp ln
	sqrt tan cosh sinh tanh hypot gamma lgamma log10 log2 log1p ldexp cube
	> < <= >= = and or round ceiling floor max min
	))


  (define (add-primitives! env)
    (let ((prim-exprs
           '(fp+ fp- fp* fp/ expt fpneg
                 abs atan asin acos sin cos exp log sqrt tan 
                 cosh sinh tanh hypot gamma lgamma log10 log2 log1p ldexp
                 (lambda (x) (* x x x))
                 fp> fp< fp<= fp>= fp=
                 (lambda (x y) (and x y)) (lambda (x y) (or x y)) 
                 round ceiling floor fpmax fpmin
                 )))
      (for-each (lambda (n v qb fms rt) 
                  (let ((fb (extend-procedure 
                             v `((name ,n) (eval-body ,qb)
                                 (rt ,rt) (formals ,fms)))))
                    (hash-table-set! env n fb)))
                builtin-fns
                (apply eval-math prim-exprs)
                prim-exprs
                `((,fptype ,fptype) (,fptype ,fptype) (,fptype ,fptype) (,fptype ,fptype) 
                  (,fptype ,fptype) (,fptype)
                  (,fptype) (,fptype) (,fptype) (,fptype) (,fptype) (,fptype) (,fptype)
                  (,fptype) (,fptype) (,fptype)
                  (,fptype) (,fptype) (,fptype) (,fptype) (,fptype) (,fptype) (,fptype)
                  (,fptype) (,fptype) (,fptype)
                  (,fptype) 
                  (bool bool) (bool bool) (bool bool) (bool bool) (bool bool) (bool bool) (bool bool) 
                  (,fptype) (,fptype) (,fptype) (,fptype ,fptype) (,fptype ,fptype) 
                  (,fpvector-type integer) )
                `(,fptype ,fptype ,fptype ,fptype 
                          ,fptype ,fptype
                          ,fptype ,fptype ,fptype ,fptype ,fptype ,fptype ,fptype
                          ,fptype ,fptype ,fptype
                          ,fptype ,fptype ,fptype ,fptype ,fptype ,fptype ,fptype
                          ,fptype ,fptype ,fptype
                          ,fptype 
                          bool bool bool bool bool bool bool 
                          ,fptype ,fptype ,fptype ,fptype ,fptype 
                          ,fptype )
                )))


  (define (add-constants! env)
    (for-each (lambda (kv) (hash-table-set! env (car kv) (cadr kv)))
              nemo:math-constants))


  (define (add-units! env)
    (for-each (lambda (kv) (hash-table-set! env (car kv) (cadr kv)))
              nemo:basic-units))


  (define (enumdeps expr)
    (let loop ((expr expr) (ax (list)) (lbs (list)))
      (match expr 
	     (('let bs e)  (let let-loop ((bs bs) (ax ax) (lbs lbs))
			     (if (null? bs) (loop e ax lbs)
				 (let ((x   (first  (car bs)))
				       (ex  (second (car bs))))
				   (let* ((lbs1  (cons x lbs))
					  (ax1   (loop ex ax lbs)))
				     (let-loop (cdr bs) ax1 lbs1))))))

	     ((s . es)     (if (symbol? s) (fold (lambda (e ax) (loop e ax lbs)) ax es) 
			       (fold (lambda (e ax) (loop e ax lbs)) ax (cons s es))))
	     (id           (if (and (symbol? id) (not (member id lbs))) (cons id ax) ax)))))


  (define (binop-fold op lst)
    (if (null? lst) lst
	(match lst
	       ((x)   x)
	       ((x y) `(,op ,x ,y))
	       ((x y . rest) `(,op (,op ,x ,y) ,(binop-fold op rest)))
	       ((x . rest) `(,op ,x ,(binop-fold op rest))))))


  ;; if argument is constant, return the negative of that constant,
  ;; otherwise return `(neg ,expr)
  (define (negate expr)
    (if (number? expr) (- expr)
	`(neg ,expr)))

  ;; 1. make sure all constants in an expression are flonums
  ;; 2. fold expressions like (+ a b c d) into nested binops 
  (define (make-normalize-expr arity-check symbol-check)
    (lambda (expr loc)
      (let recur ((expr expr) (lbs '()))
	(match expr 
	       (('let bs e)         (let ((normalize-bnd  (lambda (lbs) (lambda (x) `(,(first x) ,(recur (second x) lbs)))))
                                          (lbs1 (append (map first bs) lbs)))
				      `(let ,(map (normalize-bnd lbs1) bs) ,(recur e lbs1))))
	       (('if c t e)         `(if ,(recur c lbs) ,(recur t lbs) ,(recur e lbs))) 
	       (('+ . es)           (binop-fold '+ (map (lambda (x) (recur x lbs)) es)))
	       (('- . es)           (let ((es1 (map (lambda (x) (recur x lbs)) es)))
				      (binop-fold '+ (cons (car es1) (map negate (cdr es1))))))
	       (('* . es)           (binop-fold '* (map (lambda (x) (recur x lbs)) es)))
	       (('/ . es)           (binop-fold '/ (map (lambda (x) (recur x lbs)) es)))
	       (('fix n)            n)
	       ((s . es)            (begin
				      (arity-check s es loc)
				      (cons s (map (lambda (x) (recur x lbs)) es))))
	       (x                   (cond ((number? x) (exact->inexact x))
                                          ((symbol? x) (begin (symbol-check x loc lbs) x))
                                          (else x)))
                                                              
               ))))

  (define base-env
    (let ((env (make-hash-table)))
      (add-primitives! env)
      (add-constants! env)
      (add-units! env)
      env))
    
  (define (make-const-env nemo-env)
    (let ((env (nemo:env-copy base-env)))
      (hash-table-for-each nemo-env
        (lambda (sym en)
	  (cond  ((nemo:quantity? en)  
		  (cases nemo:quantity en
			 (CONST (name value u)  
				(hash-table-set! env name value))
			 (PRIM (name value)  
			       (hash-table-set! env name value))))
		 ((procedure? en)
		  (hash-table-set! env sym en)))))
	env))


  (define (const-env-entry->value en)
    (cond  ((nemo:quantity? en)  
            (cases nemo:quantity en
                   (CONST (name value u)  value)
                   (PRIM (name value)  value)
                   ))
           ((procedure? en)  en)
           ((or (number? en) (symbol? en))   en)
           (else #f)))

  (define (system name)
    (let ((env  (nemo:env-copy base-env))
	  (name (if (symbol? name) name (string->symbol name))))
      (hash-table-set! env (nemo-intern 'dispatch)  (DISPATCH nemo-dispatch))
      (hash-table-set! env (nemo-intern 'name)      (SYSNAME name))
      (hash-table-set! env (nemo-intern 'exports)   (EXPORTS (list)))
      (hash-table-set! env (nemo-intern 'toplevel)  (COMPONENT 'toplevel 'toplevel (list) (list)))
      env))

  (define (add-external! nemo-env)
    (lambda (sym typ)
      (match typ
	     ('output
	      (begin
		(if (not (hash-table-exists? nemo-env sym))
		    (nemo:error 'add-external! ": exported quantity " sym " is not defined"))
		(let* ((exports-sym   (nemo-intern 'exports))
		       (exports       (hash-table-ref nemo-env exports-sym)))
		  (cases nemo:quantity exports
			 (EXPORTS (lst) (hash-table-set! nemo-env exports-sym (EXPORTS (append lst (list sym)))))
			 (else  (nemo:error 'add-external! ": invalid exports entry " exports))))))
	     
	     (('input sym lsym ns . rest)
	      (let (
                    (lsym (or lsym sym))
                    (usym (lookup-def 'unit rest))
                    )
		(if (hash-table-exists? nemo-env lsym)
		    (nemo:error 'add-import! ": import symbol " lsym " is already defined"))
		
		((env-extend! nemo-env) lsym '(external) 'none 
                 `(name ,sym) `(namespace ,ns) `(unit ,usym))))
	     
	     )))


  (define (make-symbol-check nemo-env)
    (lambda (s loc . rest)
      (let-optionals rest ((lbs '()))

        (if (and (not (hash-table-exists? nemo-env s)) 
                 (not (member s lbs)))
	    (begin
	      (pp (hash-table->alist nemo-env))
	      (nemo:error 'symbol-check: s " in the definition of " loc " is not defined")
	      )
            ))
      ))


  (define (make-arity-check nemo-env)
    (lambda (s args loc)
      (if (hash-table-exists? nemo-env s)
          (let ((op (hash-table-ref nemo-env s)))
            (if (extended-procedure? op)
                (let* ((fd   (procedure-data op))
                       (fms   (lookup-def 'formals fd)))
                  
                  (if (not (= (length fms) (length args)))
                      (nemo:error 'arity-check: "procedure " s 
                                  " called with incorrect number of arguments: "
                                  args)))))
          (nemo:error 'arity-check: "symbol " s "(" loc ")" " is not defined")
          )))


  (define (env-extend! nemo-env)
    (lambda (name type initial . alst)
       (let* ((sym (if (symbol? name) name (string->symbol name)))
	      (arity-check (make-arity-check nemo-env))
	      (symbol-check (make-symbol-check nemo-env))
	      (normalize-expr (make-normalize-expr arity-check symbol-check)))

	(if (hash-table-exists? nemo-env sym)
	    (nemo:error 'env-extend! ": quantity " sym " already defined")
	    (match type

	      (('label)   (begin
			    (if (not (symbol? initial)) 
				(nemo:error 'env-extend! ": label definitions require symbolic value"))
			    (hash-table-set! nemo-env sym (LABEL initial))))

	      (('external)  (begin
			      (let* (
                                     (usym (lookup-def 'unit alst))
                                     (u    (or (and usym 
                                                    (let ((v (hash-table-ref nemo-env usym)))
                                                      (or v (nemo:error 'env-extend! ": invalid unit name in constant definition" name usym))))
                                               nemo:unitless))
                                     (ns             (lookup-def 'namespace alst))
                                     (external-name  (lookup-def 'name alst))
				     (x              (EXTERNAL name external-name ns u))
                                     )
				(hash-table-set! nemo-env sym x)
				)))
			      
	      (('prim)    (let* ((rhs (lookup-def 'rhs alst))
				 (val (if (and rhs (procedure? initial) )
					  (extend-procedure initial rhs)
					  initial)))
			    (hash-table-set! nemo-env sym (PRIM name val ))))

	      (('const)   (let* ((usym (lookup-def 'unit alst))
                                 (u    (or (and usym (let ((v (hash-table-ref nemo-env usym)))
                                                       (or v (nemo:error 'env-extend! ": invalid unit name in constant definition" name usym))))
                                           nemo:unitless)))
			    (if (not (number? initial))
				(nemo:error 'env-extend! ": constant definitions require numeric value" name initial))
			    (hash-table-set! nemo-env sym (CONST name initial u))
                            ))

	      (('asgn)    (let* ((usym (lookup-def 'unit alst))
                                 (u    (or (and usym (let ((v (hash-table-ref nemo-env usym)))
                                                       (or v (nemo:error 'env-extend! ": invalid unit name in assignment definition" name usym))))
                                           nemo:unitless))
                                 (rhs (lookup-def 'rhs alst)))

			    (if (not (eq? initial 'none))
				(nemo:error 'env-extend! 
						    ": state function definitions must have initial value of '(none)"))
			    (if (not rhs) 
				(nemo:error 'env-extend! ": state function definitions require an equation"))
                            (let ((expr1 (normalize-expr rhs (sprintf "assignment ~A" sym))))
                              (hash-table-set! nemo-env sym (ASGN name 0.0 expr1 u)))
                            ))

	      (('rate)    (let* ((usym (lookup-def 'unit alst))
                                 (u    (or (and usym (let ((v (hash-table-ref nemo-env usym)))
                                                       (or v (nemo:error 'env-extend! ": invalid unit name in rate equation definition" name usym))))
                                           nemo:unitless))
                                 (rhs (lookup-def 'rhs alst))
                                 (power (lookup-def 'power alst))
                                 (local-env (let ((local-env (hash-table-copy nemo-env)))
                                              (hash-table-set! local-env name #t)
                                              local-env))
                                 (symbol-check (make-symbol-check local-env))
                                 (normalize-expr (make-normalize-expr arity-check symbol-check))
                                 
                                 )

			    (if (not (rhs? rhs))
				(nemo:error 'env-extend! ": rate equation definitions require an equation"))

                            (let ((initial-expr
                                   (and initial
                                        (normalize-expr initial
                                                        (sprintf "initial value for rate equation ~A" sym))))
                                  (rhs-expr (normalize-expr rhs (sprintf "rate equation ~A" sym))))
                              (hash-table-set! nemo-env sym (RATE name initial-expr rhs-expr power u)))

                            ))

	      (('transient)  (let* ((usym (lookup-def 'unit alst))
                                    (u    (or (and usym (let ((v (hash-table-ref nemo-env usym)))
                                                          (or v (nemo:error 'env-extend! ": invalid unit name in transient definition" name usym))))
                                              nemo:unitless))                                    
                                    (rhs     (lookup-def 'rhs alst))
                                    (asgn    (lookup-def 'asgn alst))
                                    (power   (lookup-def 'power alst))
                                    (local-env (let ((local-env (hash-table-copy nemo-env)))
                                                 (hash-table-set! local-env name #t)
                                                 local-env))
                                    (symbol-check (make-symbol-check local-env))
                                    (normalize-expr (make-normalize-expr arity-check symbol-check))
                                    
                                    )

			    (if (not (rhs? rhs))
				(nemo:error 'env-extend! ": transient definitions require a rate equation"))

			    (if (not (rhs? asgn))
				(nemo:error 'env-extend! ": transient definitions require an event assignment"))

                            (let ((initial-expr
                                   (and initial
                                        (normalize-expr initial
                                                        (sprintf "initial value for transient ~A" sym))))
                                  (rhs-expr (normalize-expr rhs (sprintf "transient ~A" sym)))
                                  (asgn-expr (normalize-expr asgn (sprintf "transient ~A assignment" sym)))
                                  )
                              (hash-table-set! nemo-env sym (TRANSIENT name initial-expr rhs-expr
                                                                       asgn-expr power u)))

                            ))

	      (('reaction)  (begin
			    (let* ((usym   (lookup-def 'unit alst))
                                   (u      (or (and usym
                                                    (let ((v (hash-table-ref nemo-env usym)))
                                                      (or v (nemo:error 'env-extend! ": invalid unit name in reaction definition" name usym))))
                                               nemo:unitless))
                                   (power  (or (lookup-def 'power alst) 1))
                                   (transitions   
				   (map (lambda (t) 
					  (match t
						 (( '<-> (and src (? symbol?)) (and dst (? symbol?)) r1 r2)  
                                                  (let ((r1-expr
                                                         (normalize-expr 
                                                          r1 (sprintf "forward transition rate between states ~A and ~A in reaction ~A "
                                                                      src dst sym)))
                                                        (r2-expr
                                                         (normalize-expr 
                                                          r2 (sprintf "backward transition rate between states ~A and ~A in reaction ~A "
                                                                      src dst sym)))
                                                        )
						  `( <-> ,src ,dst ,r1-expr ,r2-expr)))

						 (( '-> (and src (? symbol?)) (and dst (? symbol?)) r1)  
                                                  (let ((r1-expr
                                                         (normalize-expr 
                                                          r1 (sprintf "transition rate between states ~A and ~A in reaction ~A "
                                                                      src dst sym))))
						  `( -> ,src ,dst ,r1-expr) ))

						 (else 
						  (nemo:error 'env-extend! ": invalid transition " t))))
					(or (alist-ref 'transitions alst) (list))))
				  (conserve      (lookup-def 'conserve alst))
				  (open          (lookup-def 'open alst)))
			      (if (null? transitions)
				  (nemo:error 'env-extend! 
					      ": transition state complex definitions require a transition scheme"))
			      (if (not open) 
				  (nemo:error 'env-extend! ": state complex definitions require open state"))
			      (if (not (integer? power))
				  (nemo:error 'env-extend!
					      ": definition for state " sym
					      " requires an integer power (" power  " was given)"))
			      
			      (let ((en (REACTION name (and initial (normalize-expr initial (sprintf "initial value for reaction ~A" sym)))
                                                  open transitions 
                                                  (and conserve (list conserve)) power u)))
				(hash-table-set! nemo-env sym en)))))


	      (else       (begin 
			    (hash-table-set! nemo-env sym `(,type (name ,sym) . ,initial))))
	      )))))

  (define (infer nemo-env ftenv body)
    (let recur ((expr body) (lb (list)))
      (match expr 
	     (('if c t e)
	      (let ((ct (recur c lb))
		    (tt (recur t lb))
		    (et (recur e lb)))
		(and ct tt et 
		     (begin
		       (if (not (equal? ct 'bool)) 
			   (nemo:error 'infer "if condition type must be boolean"))
		       (if (equal? tt et) tt
			   (nemo:error 'infer "type mismatch in if statement: then = " tt
				      " else = " et))))))
	     (('let bs e)
	      (let* ((rlb (lambda (x) (recur x lb)))
		     (tbs (map rlb (map second bs)))
		     (lb1 (append (zip (map first bs) tbs) lb)))
		(recur e lb1)))
	     
	     ((s . es)    
	      (let* ((f    (hash-table-ref nemo-env s))
		     (lst  (procedure-data f)))
		(and lst 
		     (let ((rt   (lookup-def 'rt   lst))
			   (fms  (lookup-def 'formals lst)))
		       (and rt fms
			    (begin
			      (for-each (lambda (x ft)
					  (if (and (symbol? x) (not (hash-table-exists? ftenv x)))
					      (hash-table-set! ftenv x ft)))
					es fms)
			      (let* ((rlb (lambda (x) (recur x lb)))
				     (ets (map rlb es)))
				(and (every identity ets)
				     (every (lambda (xt ft) (equal? xt ft)) ets fms)
				     rt))))))))
	     
	     (id    (cond ((symbol? id)     (or (lookup-def id lb) (hash-table-ref ftenv id)))
			  ((number? id)     fptype)
			  ((boolean? id)    'bool)
			  ((fpvector? id)   fpvector-type)
			  (else #f))))))
    

  (define (defun! nemo-env)

    (lambda (name formals body)

	(let* ((const-env (make-const-env nemo-env))
               (local-env (let ((local-env (hash-table-copy nemo-env)))
                            (for-each (lambda (s) (hash-table-set! local-env s #t))  formals)
                            local-env))
               (arity-check (make-arity-check local-env))
               (symbol-check (make-symbol-check local-env))
               (normalize-expr (make-normalize-expr arity-check symbol-check))
               (sym (if (symbol? name) name (string->symbol name))))

	  (letrec ((enumconsts
		    (lambda (lb)
		      (lambda (expr ax)
			(match expr 
			       (('let bs e)  (let ((ec (enumconsts (append (map first bs) lb))))
					       (ec e (fold ec ax (map second bs)))))

			       (('if . es)   (fold (enumconsts lb) ax es))

			       ((s . es)     (cond ((and (symbol? s) (not (member s builtin-fns))
							 (hash-table-exists? const-env s))
                                                    (let ((v (const-env-entry->value (hash-table-ref const-env s))))
                                                      (cons (cons s v) (fold (enumconsts lb) ax es))))

                                                   ((and (symbol? s) (not (member s builtin-fns)) (not (member s lb)))
                                                    (nemo:error 'defun ": quantity " s " not defined"))

                                                   (else (fold (enumconsts lb) ax es))
                                                   ))

			       (s            (cond 
					      ((and (symbol? s) (not (member s lb)) 
						    (hash-table-exists? const-env s))
					       (let ((v (const-env-entry->value (hash-table-ref const-env s))))
						 (cons (cons s v) ax) ))

						((and (symbol? s) (not (member s lb)))
						 (nemo:error 'defun ": quantity " s " not defined"))

						(else ax)))

                               ))
                      ))
                   )


	    (if (hash-table-exists? nemo-env sym)
		(nemo:error 'defun! ": quantity " sym " already defined")
		(let* (
                       (body    (normalize-expr body (sprintf "function ~A" sym)))
		       (consts  (delete-duplicates ((enumconsts formals) body (list)) 
                                                   (lambda (x y) (equal? (car x) (car y)))))
		       (eval-body (let ((svs (map (lambda (sv)  
						    (let ((s (car sv))
							  (v (if (procedure? (cdr sv)) 
								 (lookup-def 'eval-body (procedure-data (cdr sv)))
								 (cdr sv))))
						      `(,s ,v))) consts)))
				    (if (null? svs) `(lambda ,formals ,body)
				    `(let ,svs (lambda ,formals ,body)))))
		       (f      (eval eval-body))
                       )

		  (let* ((ftenv  (make-hash-table))
			 (rt     (infer nemo-env ftenv body))
			 (ftypes (filter-map (lambda (x) 
					       (or (and (hash-table-exists? ftenv x)
							(hash-table-ref ftenv x)) 'double))
					     formals))
			 (ef     (extend-procedure f 
				   `((name ,sym) (body ,body) (eval-body ,eval-body) 
                                     (rt ,rt) (formals ,ftypes) (vars ,formals)
				     (consts ,(filter (lambda (x) (not (member x builtin-fns))) consts)))))
			 )

		  (hash-table-set! nemo-env sym ef))
		  ))
	    ))
	))

  (define (symbol-list? lst)
    (and (list? lst) (every symbol? lst)))

  (define (sysname nemo-env)
    (let ((v (hash-table-ref nemo-env (nemo-intern 'name))))
      (and v (cases nemo:quantity v
                    (SYSNAME (name)  name)))
      ))
         

  (define (extended nemo-env)
      (filter-map (lambda (sym) 
		    (let ((x (hash-table-ref nemo-env sym)))
		      (and (not (nemo:quantity? x)) (not (procedure? x)) 
			   (match x 
				  (((? symbol-list?) ('name name) . rest)  `(,sym ,x))
				  (else #f)))))
	   (hash-table-keys nemo-env)))
			

  (define (extended-with-tag nemo-env tag)
      (filter-map (lambda (sym) 
		    (let ((x (hash-table-ref nemo-env sym)))
		      (and (not (nemo:quantity? x)) (not (procedure? x)) 
			   (match x 
				  (((? (lambda (x) (equal? x tag))) ('name name) . rest)  
				   `(,sym ,x))
				  (else #f)))))
	   (hash-table-keys nemo-env)))
			

  (define (components nemo-env)
      (filter-map (lambda (sym) 
		    (let ((x (hash-table-ref nemo-env sym)))
		      (and (nemo:quantity? x)
			   (cases nemo:quantity x
				  (COMPONENT (name type lst _)  `(,name ,type ,sym))
				  (else #f)))))
	   (hash-table-keys nemo-env)))


  (define (component-name nemo-env sym)
    (let ((x (hash-table-ref nemo-env sym)))
      (and (nemo:quantity? x)
	   (cases nemo:quantity x
		  (COMPONENT (name type lst _)  name)
		  (else #f)))))


  (define (component-symbols nemo-env sym)
    (let ((x (hash-table-ref nemo-env sym)))
      (and (nemo:quantity? x)
	   (cases nemo:quantity x
		  (COMPONENT (name type lst _)  lst)
		  (else #f)))))


  (define (component-scope-subst nemo-env sym)
    (let ((x (hash-table-ref nemo-env sym)))
      (and (nemo:quantity? x)
	   (cases nemo:quantity x
		  (COMPONENT (name type lst scope-subst)  scope-subst)
		  (else #f)))))


  (define (component-exports nemo-env sym)
    (let ((all-exports (cases nemo:quantity (hash-table-ref nemo-env (nemo-intern 'exports))
			      (EXPORTS (lst)  lst))))
      (let ((x  (hash-table-ref nemo-env sym)))
	(and (nemo:quantity? x)
	     (cases nemo:quantity x
		    (COMPONENT (name type lst _)  
                               (begin
                                 (filter (lambda (x) (member x lst)) all-exports)))
		    (else #f))))))


  (define (component-subcomps nemo-env sym)

    (define (component-type x)
      (cases nemo:quantity x
	     (COMPONENT (name type lst _) type)
	     (else #f)))

    (define (component-name x)
      (cases nemo:quantity x
	     (COMPONENT (name type lst _) name)
	     (else #f)))

    (let ((en (hash-table-ref nemo-env sym)))
      (and (nemo:quantity? en)
	   (cases nemo:quantity en
		  (COMPONENT (name type lst _)  
			     (filter-map 
			      (lambda (s) 
				(let ((x (hash-table-ref nemo-env s)))
				  (and (iscomp? x) `(,(component-type x) ,(component-name x) ,s)))) lst))
		  (else #f)))))

  (define (component-extend! nemo-env)
    (lambda (comp-name sym)
      (let ((x (hash-table-ref nemo-env comp-name)))
	(if (nemo:quantity? x)
	    (cases nemo:quantity x
		   (COMPONENT (name type lst scope-subst)  
			      (let ((en1 (COMPONENT name type (delete-duplicates (append lst (list sym))) scope-subst)))
				(hash-table-set! nemo-env comp-name en1)))
		   (else (nemo:error 'component-extend! ": invalid component " comp-name)))
	    (nemo:error 'component-extend! ": invalid component " comp-name)))))


  (define (component-enumdeps nemo-env sym)

    (let ((x (hash-table-ref nemo-env sym)))
      (and (nemo:quantity? x)
	   (cases nemo:quantity x

		  (COMPONENT 
		   (name type lst scope-subst)  
		   (delete-duplicates
		    (append
		     (fold (lambda (qsym ax)
			     (let* ((q   (hash-table-ref nemo-env qsym))
				    (rhs (qrhs q)))
			       (or (and rhs (append (enumdeps rhs) ax)) ax)))
			   '()
			   lst)
		     (fold (lambda (x ax) (append (component-enumdeps nemo-env (third x)) ax))
                           '()
                           (component-subcomps nemo-env sym)))))

		  (else #f)))))


  (define (component-env nemo-env sym . syms)
    (fold 
     (lambda (sym env)
       (let ((comp (hash-table-ref nemo-env sym)))
	 (and (nemo:quantity? comp)
	      (cases nemo:quantity comp
		     (COMPONENT 
		      (name type lst scope-subst)  
		      (let* ((depnames (component-enumdeps nemo-env sym))
			     (subnames (map third (component-subcomps nemo-env sym)))
			     (cnames   lst))
			(let* ((syms (delete-duplicates (append depnames subnames cnames)))
			       (vals (map (lambda (x) (hash-table-ref nemo-env x)) syms)))
			  (for-each (lambda (s v) (hash-table-set! env s v)) 
				    syms vals)
			  env
			  )))
		     (else env)))))
     (nemo:env-copy base-env)
     (cons sym syms)))


  (define (exports nemo-env)
    (cases nemo:quantity (hash-table-ref nemo-env (nemo-intern 'exports))
	   (EXPORTS (lst)  lst)))


  (define (imports nemo-env)
      (filter-map (lambda (sym) 
		    (let ((x (hash-table-ref nemo-env sym)))
		      (and (nemo:quantity? x)
			   (cases nemo:quantity x
				  (EXTERNAL (local-name name namespace u)  (list local-name name namespace))
				  (else #f)))))
	   (hash-table-keys nemo-env)))



  (define (consts nemo-env)
      (filter-map (lambda (sym) 
		    (let ((x (hash-table-ref nemo-env sym)))
		      (and (nemo:quantity? x)
			   (cases nemo:quantity x
				  (CONST (name value u)  (list name value) )
				  (else #f)))))
	   (hash-table-keys nemo-env)))



  (define (states nemo-env)
    (fold (lambda (sym ax) 
            (let ((x (hash-table-ref nemo-env sym)))
              (if (nemo:quantity? x)
                  (cases nemo:quantity x
                         (REACTION (name initial open transitions conserve power u)
                                   (let* ((ss1 (delete-duplicates (append (map second transitions) 
                                                                          (map third transitions))))
                                          (ss2 (map (lambda (x) (list name x))  ss1)))
                                     (append ss2 ax)))
                         (RATE (name initial rhs _ u) 
                               (cons (list #f name) ax))
                         (TRANSIENT  (name initial rhs asgn power u)  
                                     (cons (list #f name) ax))
                         (else ax))
                  ax)))
          (list) (hash-table-keys nemo-env)))


  (define (reactions nemo-env)
      (fold (lambda (sym ax) 
		    (let ((x (hash-table-ref nemo-env sym)))
		      (if (nemo:quantity? x)
			   (cases nemo:quantity x
				  (REACTION (name initial open transitions conserve power u)
					  (cons name ax))
				  (else ax))
			   ax)))
	   (list) (hash-table-keys nemo-env)))


  (define (rates nemo-env)
      (filter-map (lambda (sym) 
		    (let ((x (hash-table-ref nemo-env sym)))
		      (and (nemo:quantity? x)
			   (cases nemo:quantity x
				  (RATE (name value rhs _ u) name)
				  (else #f)))))
	   (hash-table-keys nemo-env)))


  (define (transients nemo-env)
      (filter-map (lambda (sym) 
		    (let ((x (hash-table-ref nemo-env sym)))
		      (and (nemo:quantity? x)
			   (cases nemo:quantity x
                                  (TRANSIENT  (name initial rhs asgn power u) name)
				  (else #f)))))
	   (hash-table-keys nemo-env)))


  (define (asgns nemo-env)
      (filter-map (lambda (sym) 
		    (let ((x (hash-table-ref nemo-env sym)))
		      (and (nemo:quantity? x)
			   (cases nemo:quantity x
				  (ASGN (name value rhs) name)
				  (else #f)))))
	   (hash-table-keys nemo-env)))


  (define (defuns nemo-env)
      (filter-map (lambda (sym) 
		    (let ((x (hash-table-ref nemo-env sym)))
		      (and (procedure? x) (not (member sym builtin-fns)) (list sym x))))
	   (hash-table-keys nemo-env)))


  (define (toplevel nemo-env)
    (cases nemo:quantity (hash-table-ref nemo-env (nemo-intern 'toplevel))
	   (COMPONENT (name type lst _) `(,type ,lst))))


				       
  (define (exam nemo-env)
    (lambda (name)
      (let ((sym (if (symbol? name) name (string->symbol name)))
	    (out (current-output-port)))
	(if (not (hash-table-exists? nemo-env sym))
	    (nemo:error 'exam ": quantity " sym " is not defined")
	    (let ((x (hash-table-ref nemo-env sym)))
	      (cases nemo:quantity x
		     (LABEL  (v)
			    (begin
			      (fprintf out "~a: label\n" name)
			      (fprintf out "    value: ~a\n" v)))

		     (PRIM  (name value)
			    (begin
			      (fprintf out "~a: compiled nemo primitive\n" name)
			      (fprintf out "    value: ~a\n" value)))

		     (CONST    (name value u)
			       (begin
				 (fprintf out "~a: constant\n" name)
				 (fprintf out "    value: ~a\n" value)))
		     
		     (ASGN     (name value rhs)
			       (begin
				 (fprintf out "~a: state function\n" name)
				 (fprintf out "    value: ~a\n" value)))

		     (REACTION (name initial open transitions conserve power u)
			     (begin
			       (fprintf out "~a: transition state complex\n" name)
			       (fprintf out "    initial value: ~a\n" initial)))
		     
		     (RATE     (name initial rhs power u)
			       (begin
				 (fprintf out "~a: rate equation\n" name)
				 (fprintf out "    rhs: ~a\n" rhs)
				 (if power (fprintf out "    power: ~a\n" power))
				 ))

		     (TRANSIENT  (name initial rhs asgn power u) 
                                 (begin
                                   (fprintf out "~a: transient\n" name)
                                   (fprintf out "    rhs: ~a\n" rhs)
                                   (fprintf out "    assignment: ~a\n" asgn)
                                   (if power (fprintf out "    power: ~a\n" power))
                                   ))

		     (else (nemo:error 'exam name ": unknown type of quantity"))))))))
  
  (define (eval-simple-expr env expr)
    (cond ((number? expr) expr)
	  ((symbol? expr) (hash-table-ref env expr))
	  ((pair? expr)   (match expr
                                 (('if cexpr then-expr else-expr)
                                  (let ((cval (eval-simple-expr env cexpr))
                                        (then-val (eval-simple-expr env then-expr))
                                        (else-val (eval-simple-expr env else-expr))
                                        )
                                    (if cval then-val else-val)
                                    ))
                                 (else
                                  (let ((expr1 (map (lambda (x) (eval-simple-expr env x)) expr)))
                                    (apply (car expr1) (cdr expr1))))))
          ))

  (define (eval-const nemo-env expr qname)
    (let* ((arity-check (make-arity-check nemo-env))
           (symbol-check (make-symbol-check nemo-env))
	   (normalize-expr (make-normalize-expr arity-check symbol-check)))
      (let ((expr1 (normalize-expr expr (sprintf "constant ~A" qname)))
	    (const-env (make-const-env nemo-env)))
	(condition-case
	 (exact->inexact (eval-simple-expr const-env expr1))
	 [var () expr1])
	)))


  (define (iscomp? x)
    (cond ((nemo:quantity? x)
	   (cases nemo:quantity x
		  (COMPONENT  (name type lst _)  #t)
		  (else #f)))
	  (else #f)))

  (define (isdep? x)
    (cond ((nemo:quantity? x)
	   (cases nemo:quantity x
		  (ASGN  (name value rhs)  #t)
		  (else #f)))
	  ((and (list? x) (every pair? (cdr x)))  (alist-ref 'dep?  (cdr x)))
	  (else #f)))


  (define (isstate? x)
    (and (nemo:quantity? x)
	 (cases nemo:quantity x
		(REACTION  (name initial open transitions u)  #t)
		(RATE      (name initial rhs _ u) #t)
		(TRANSIENT (name initial rhs _ u) #t)
		(else #f))
         ))


  (define (qrhs x)
    (and (nemo:quantity? x)
	 (cases nemo:quantity x
		(REACTION (name initial open transitions u)  
                          (map cadddr transitions))
		(RATE  (name initial rhs _ u)  
                       rhs)
                (TRANSIENT  (name initial rhs asgn power u) 
                            (list rhs asgn))
		(ASGN  (name value rhs)  
                       rhs)
		(else #f))))


  ;; create equation dependency graph
  (define (make-eqng nemo-env)
    (let* ((sysname    (sysname nemo-env))
	   (g          (make-digraph sysname (string-append (symbol->string sysname) 
							    " equation dependency graph")))
	   (add-node!  (g 'add-node!))
	   (add-edge!  (g 'add-edge!))
	   (nemo-list  (filter (lambda (sym) (let ((x (hash-table-ref nemo-env sym)))
					       (or (isstate? x) (isdep? x))))
			       (hash-table-keys nemo-env)))
	   (nemo-ids      (list-tabulate (length nemo-list) identity))
	   (name->id-map  (zip nemo-list nemo-ids)))
      (let-values (((state-list asgn-list)  
		    (partition (lambda (sym) (isstate? (hash-table-ref nemo-env sym)))
			       nemo-list)))
		  
	 ;; insert equations in the dependency graph
         (for-each (lambda (i n) (add-node! i n)) nemo-ids nemo-list)
	 ;; create dependency edges in the graph
	 (for-each (lambda (e) 
		     (match e ((ni . nj) (begin
					   (let ((i (car (alist-ref ni name->id-map)))
						 (j (car (alist-ref nj name->id-map))))
					     (add-edge! (list i j (format "~A=>~A" ni nj))))))
			    (else (nemo:error 'make-eqng ": invalid edge " e))))
		   (fold (lambda (qsym ax) 
			   (let* ((q   (hash-table-ref nemo-env qsym))
				  (rhs (qrhs q)))
			     (if rhs 
				 (let* ((deps (filter (if (isstate? q)
							  (lambda (sym) 
							    (if (not (hash-table-exists? nemo-env sym))
								(nemo:error 'make-eqng ": undefined symbol " sym 
									    " in definition of quantity " qsym))
							    (and (let ((x (hash-table-ref nemo-env sym)))
								   (and (isdep? x) (not (eq? sym qsym))))))
							  (lambda (sym) 
							    (if (not (hash-table-exists? nemo-env sym))
								(nemo:error 'make-eqng ": undefined symbol " sym 
									    " in definition of quantity " qsym))
							    (and (let ((x (hash-table-ref nemo-env sym)))
								   (isdep? x)))))
						      (enumdeps rhs)))
					  (edges (map (lambda (d) (cons d qsym)) deps)))
				   (if edges (append edges ax) ax))
				 ax)))
			 (list) nemo-list))
	 (let ((cycles (graph-cycles-fold g (lambda (cycle ax) (cons cycle ax)) (list))))
	   (if (null? cycles) (list state-list asgn-list g)
	       (nemo:error 'make-eqng ": equation cycle detected: " (car cycles)))))))


  ;; given a graph, create a partial ordering based on BFS distance from root
  (define (graph->bfs-dist-poset g)
    (define node-info (g 'node-info))
    (let-values (((dists dmax) (graph-bfs-dist g ((g 'roots)))))
      (let loop ((poset  (make-vector (+ 1 dmax) (list)))
		 (i      (- (s32vector-length dists) 1)))
	(if (>= i 0)
	    (let* ((c     (s32vector-ref dists i))
		   (info  (node-info i)))
	      (vector-set! poset c (cons (cons i info) (vector-ref poset c)))
	      (loop poset (- i 1)))
	    (begin
	      poset)))
      ))


  (define (make-eval-poset nemo-env eqposet)
    (vector-map 
       (lambda (i lst) 
	 (filter-map (lambda (id+sym)
		       (let* ((sym  (cdr id+sym))
			      (x    (hash-table-ref nemo-env sym)))
			 (and (nemo:quantity? x)
			      (cases nemo:quantity x
				     (REACTION (name initial open transitions u) 
					     (let ((rs (map cadddr transitions)))
					       (list 're sym rs)))
				     (RATE  (name initial rhs _ u)
					    (list 'r sym rhs))
				     (TRANSIENT  (name initial rhs asgn power u) 
                                                 (list 'tra sym rhs asgn))
				     (ASGN  (name value rhs)
					    (list 'a sym rhs))
				     (else nemo:error 'make-eval-poset
					   ": invalid quantity in equation poset: " sym)))))
		     lst))
       eqposet))


  (define (eval-expr env)
    (lambda (expr)
      (let ((val (match expr
			(('if c t f) 
			 (let ((ee (eval-expr env)))
			   (condition-case
			    (if (ee c) (ee t) (ee f))
			    [var () 
			       (nemo:error 'eval-expr " exception in " expr ": \n"
					  (lambda () (print-error-message var)))])))

			((s . es)   
			 (condition-case 
			  (let ((op   (hash-table-ref env s))
				(args (map (eval-expr env) es)))
			    (if (extended-procedure? op)
				(let* ((fd   (procedure-data op))
				       (vs  (lookup-def 'vars fd)))

				  (if (not (= (length vs) (length args)))
				      (nemo:error 'eval-expr "procedure " s 
						  " called with incorrect number of arguments"))))
			    (apply op args))
			  [var () 
			       (nemo:error 'eval-expr " exception in " expr ": \n"
					  (lambda () (print-error-message var)))]))
			
			(s                  
			 (cond ((symbol? s) (hash-table-ref env s))
			       ((number? s) s)
			       (else (nemo:error 'eval-expr "unknown expression " s)))))))
	val)))


  (define (depgraph nemo-env)
    (match-let (((state-list asgn-list g)  (make-eqng nemo-env))) g))


  (define (depgraph* nemo-env)
    (match-let (((state-list asgn-list g)  (make-eqng nemo-env))) 
	       (list state-list asgn-list g)))


  ;; Dispatcher
  (define (nemo-dispatch selector)
    (case selector
      ((add-external!)     add-external!)
      ((defun!)            defun!)
      ((depgraph)          depgraph)
      ((depgraph*)         depgraph*)
      ((depgraph->bfs-dist-poset)  graph->bfs-dist-poset)
      ((eval-const)        eval-const)
      ((env-extend!)       env-extend!)
      ((subst-expr)        (subst-driver (lambda (x) (and (symbol? x) x)) 
					 nemo:binding? 
					 identity 
					 nemo:bind 
					 nemo:subst-term))
      ((exam)                exam)
      ((make-const-env)      make-const-env)
      ((system)              system)
      ((sysname)             sysname)
      ((asgns)               asgns)
      ((states)              states)
      ((reactions)           reactions)
      ((rates)               rates)
      ((transients)          transients)
      ((defuns)              defuns)
      ((consts)              consts)
      ((exports)             exports)
      ((imports)             imports)
      ((toplevel)            toplevel)
      ((components)          components)
      ((component-env)       component-env)
      ((component-name)      component-name)
      ((component-symbols)   component-symbols)
      ((component-exports)   component-exports)
      ((component-subcomps)  component-subcomps)
      ((component-scope-subst)  component-scope-subst)
      ((component-extend!)   component-extend!)
      ((extended)            extended)
      ((extended-with-tag)   extended-with-tag)
      (else
       (nemo:error 'selector ": unknown message " selector " sent to an nemo-core object"))))

  nemo-dispatch)

(define (eval-nemo-system-decls nemo-core name sys declarations #!key 
                                (parse-expr (lambda (x . rest) x))
                                (initial-scope #f))
   (define (eval-const x loc) (and x ((nemo-core 'eval-const) sys x loc)))
   (define env-extend!  ((nemo-core 'env-extend!) sys))
   (define (compute-qid id scope scope-subst) (or (and scope scope-subst (nemo-scoped scope id)) id))
   (define (update-subst id qid subst) (if (equal? id qid) subst
                                           (subst-extend id qid subst) ))
   (define subst-expr  (subst-driver (lambda (x) (and (symbol? x) x)) 
				     nemo:binding? 
				     identity 
				     nemo:bind 
				     nemo:subst-term))
   (let ((res
	  (let loop ((ds declarations) (qs (list)) (scope initial-scope) (scope-subst '()))

	    (if (null? ds)  
		(let ((qs (reverse qs)))
		  (if (not scope)
		      (let* ((top-syms   ((nemo-core 'component-symbols ) sys (nemo-intern 'toplevel)))
			     (top-syms1  (append qs top-syms)))
			(hash-table-set! sys (nemo-intern 'toplevel) (COMPONENT 'toplevel 'toplevel top-syms1 '()))))
		  (list qs scope-subst))
		(let ((decl (car ds)))

		  (if (null? decl)
		      (loop (cdr ds) qs scope scope-subst)
		  (match-let 
		   (((qs1 scope-subst1)
		     (match decl

			    ;; labels
			    (((or 'label 'LABEL) (and id (? symbol?)) '= (and v (? symbol?)))
			     (let* ((qid  (compute-qid id scope scope-subst)))
			       (env-extend! qid '(label) v)
			       (list (cons qid qs) (update-subst id qid scope-subst))))
			    
			    ;; imported quantities
			    (((or 'input 'INPUT) . lst) 
			     (cond ((every (lambda (x) (or (symbol? x) (list? x))) lst)
				    (fold
				     (lambda (x ax) 
				       (match-let (((qs scope-subst) ax))
						  (match x
							 ((? symbol?) 
							  (let ((qid (compute-qid x scope scope-subst)))
							    (((nemo-core 'add-external!) sys) x `(input ,x ,qid #f))
							    (list (cons qid qs) (update-subst x qid scope-subst))))
							 ((id1 (or 'as 'AS) x1 . rest) 
							  (let ((qid (compute-qid x1 scope scope-subst)))
							    (((nemo-core 'add-external!) sys) x `(input ,id1 ,qid #f ,@rest))
							    (list (cons qid qs) (update-subst x1 qid scope-subst) )))
							 ((id1 (or 'from 'FROM) n1 . rest) 
							  (let ((qid (compute-qid id1 scope scope-subst)))
							    (((nemo-core 'add-external!) sys) x `(input ,id1 ,qid ,n1 ,@rest))
							    (list (cons qid qs) (update-subst id1 qid scope-subst) )))
							 ((id1 (or 'as 'AS) x1 (or 'from 'FROM) n1 . rest)
							  (let ((qid (compute-qid x1 scope scope-subst)))
							    (((nemo-core 'add-external!) sys) x `(input ,id1 ,qid ,n1 ,@rest))
							    (list (cons qid qs) (update-subst x1 qid scope-subst) )))
                                                         (((and id1 (? symbol?)) . rest)
							  (let ((qid (compute-qid id1 scope scope-subst)))
							    (((nemo-core 'add-external!) sys) id1 `(input ,id1 ,qid #f ,@rest))
							    (list (cons qid qs) (update-subst id1 qid scope-subst))))
							 )))
				     (list qs scope-subst) lst))
				   (else (nemo:error 'eval-nemo-system-decls 
						     "import statement must be of the form: "
						     "input id1 [as x1] ... "))))
			    
			    ;; exported quantities
			    (((or 'output 'OUTPUT) . (and lst (? (lambda (x) (every symbol? x)))))
			     (let ((lst1 (map (lambda (x) (compute-qid x scope scope-subst)) lst)))
			       (for-each (lambda (x) (((nemo-core 'add-external!) sys) x 'output)) lst1)
			       (list qs scope-subst)))
			    
			    ;; constant during integration
			    (((or 'const 'CONST) (and id (? symbol?)) '= (and expr (? expr? )) . rest)
			     (let* ((qid    (compute-qid id scope scope-subst))
				    (qexpr  (subst-expr (parse-expr expr `(const ,qid)) scope-subst))
				    (qval   (eval-const qexpr id))
                                    (alst   (filter identity rest))
                                    (u      (lookup-def 'unit alst))
                                    )
			       (apply env-extend! (cons* qid '(const) qval (or (and u `((unit ,u))) '())))
			       (list (cons qid qs) (update-subst id qid scope-subst))
                               ))

			    ;; state transition complex
			    (((or 'reaction 'REACTION) ((and id (? symbol?)) . rest) )
			     (let* ((loc          `(reaction ,id))
				    (alst         (filter identity rest))
                                    (u            (lookup-def 'unit alst))
				    (initial      (lookup-def 'initial alst))
                                    (open         (lookup-def 'open alst))
				    (conserve-eq  (lookup-def 'conserve alst))
				    (power        (lookup-def 'power alst))
				    (power-val    (if (expr? power) 
						      (eval-const (subst-expr (parse-expr power loc) scope-subst) 
                                                                  (sprintf "~A.power" id))
						      (nemo:error 'eval-nemo-system-decls 
								  "invalid power expression" power
								  " in definition of state complex" id)))
				    (transitions
				     (map (lambda (t) 
					    (match-let
					     (((src dst rate1 rate2)
					       (match t
						      (('-> a b r) (list a b r #f))
						      ((a '-> b r) (list a b r #f))
						      (('<-> a b r1 r2) (list a b r1 r2))
						      ((a '<-> b r1 r2) (list a b r1 r2))
						      (else (nemo:error "invalid transition " t
									" in definition of state complex " id))
						      )))
					     (if (and rate1 rate2)
						 (let ((loc `(,@loc (eq. ,src <-> ,dst))))
						   `( <-> ,(subst-expr src scope-subst) 
							  ,(subst-expr dst scope-subst)
							  ,(subst-expr (parse-expr rate1 loc) scope-subst)
							  ,(subst-expr (parse-expr rate2 loc) scope-subst)))
						 (let ((loc `(,@loc (eq. ,src -> ,dst))))
						   `( -> ,(subst-expr src scope-subst)
							 ,(subst-expr dst scope-subst)
							 ,(subst-expr (parse-expr rate1 loc) scope-subst))))))
					  (or (alist-ref 'transitions alst) (list)))))

			       (let ((conserve-eq 
				      (and conserve-eq
					   (let ((loc `(,@loc (cons. eqs.))))
					     (map (lambda (eq) 
						    (if (expr? (third eq))
							`(,(first eq) = 
							  ,(subst-expr (parse-expr (third eq) loc) scope-subst))
							(nemo:error 'eval-nemo-system-decls 
								    "invalid equation " eq)))
						  conserve-eq)))))
				 
				 (if (and (list? conserve-eq) (not (every conseq? conserve-eq)))
				     (nemo:error 'eval-nemo-system-decls
						 ": conservation equation for " id
						 " must be a linear equation: " conserve-eq))
				 
				 (let* ((qid          (compute-qid id scope scope-subst))
					(initial-expr (and initial 
							   (let ((loc `(,@loc (init. eq.))))
							     (subst-expr (parse-expr initial loc) scope-subst))))
					(initial-val  (and initial-expr (eval-const initial-expr
                                                                                    (sprintf "~A.initial" id)))))

                                   (apply env-extend! 
                                          (cons* qid '(reaction) initial-val 
                                                 `(power ,power-val) 
                                                 (if conserve-eq
                                                     `(conserve ,@conserve-eq)
                                                     `(conserve #f))
                                                 `(transitions ,@transitions)
                                                 `(open ,@open)
                                                 (or (and u `((unit ,u))) '())
                                                 ))

				   (list (cons qid qs) (update-subst id qid scope-subst))))))
			    
			    
			    ;; rate equation
			    (((or 'd 'D) ((and id (? symbol?))) '= (and expr (? expr?) ) . rest)

			     (let* ((qid     (compute-qid id scope scope-subst))
				    (scope-subst1 (update-subst id qid scope-subst))
				    (qexpr   (subst-expr (parse-expr expr `(rate ,id)) scope-subst1))
				    (alst    (filter identity rest))
				    (initial ((lambda (x) (and x (subst-expr (parse-expr x `(rate ,id)) scope-subst)))
					      (lookup-def 'initial alst)))
                                    (u       (lookup-def 'unit alst))
                                    )

			       (apply env-extend!
                                      (cons* qid '(rate) 
                                             (and initial (eval-const initial (sprintf "~A.initial" id)) )
                                             `(rhs ,qexpr)
                                             (or (and u `((unit ,u))) '())
                                             ))
			       (list (cons qid qs) scope-subst1)))
			    
			    ;; transient equation
			    (((or 't 'T 'transient) ((and id (? symbol?))) '= (and expr (? expr?) ) . rest)

			     (let* ((qid     (compute-qid id scope scope-subst))
				    (scope-subst1 (update-subst id qid scope-subst))
				    (qexpr   (subst-expr (parse-expr expr `(transient ,id)) scope-subst1))
				    (alst    (filter identity rest))
				    (initial ((lambda (x) (and x (subst-expr (parse-expr x `(transient ,id)) scope-subst)))
					      (lookup-def 'initial alst)))
				    (asgn    ((lambda (x) (and x (subst-expr (parse-expr x `(transient ,id)) scope-subst1)))
                                              (lookup-def 'onevent alst)))
                                    (u       (lookup-def 'unit alst))
                                    )

			       (apply env-extend! 
                                      (cons* qid '(transient) 
                                             (and initial (eval-const initial (sprintf "~A.initial" id)) )
                                             `(rhs ,qexpr)
                                             `(asgn ,asgn)
                                             (or (and u `((unit ,u)) '()))
                                             ))
					    
			       (list (cons qid qs) scope-subst1)))
			    
			    ;; algebraic assignment
			    (((and id (? symbol?)) '= (and expr (? expr?) ) . rest)
			     (let* ((qid    (compute-qid id scope scope-subst))
				    (qexpr  (subst-expr (parse-expr expr `(asgn ,id)) scope-subst))
                                    (alst   (filter identity rest))
                                    (u      (lookup-def 'unit alst))
                                    )
			       (apply env-extend! 
                                      (cons* qid '(asgn) 'none `(rhs ,qexpr) 
                                             (or (and u `((unit ,u)) '()))))
			       (list (cons qid qs) (update-subst id qid scope-subst))))
			    
			    ;; user-defined function 
			    (((or 'fun 'FUN 'rel 'REL 'defun 'DEFUN) (and id (? symbol?)) 
			      (and idlist (? (lambda (x) (every symbol? x)))) 
			      (and expr (? expr?)))

			     (let* ((qid          (compute-qid id scope scope-subst))
                                    (scope-subst1 (fold (lambda (x ax) (subst-remove x ax))
                                                        scope-subst
                                                        idlist))
				    (qexpr         (subst-expr (parse-expr expr `(defun ,qid)) 
                                                               scope-subst1))
                                    )
			       (((nemo-core 'defun!) sys) qid idlist qexpr)
			       (list (cons qid qs) (update-subst id qid scope-subst))))
			    
			    ;; compiled primitives
			    (((or 'prim 'PRIM) id value) 
			     (cond ((symbol? id)  (env-extend! id '(prim) value))
				   (else (nemo:error 'eval-nemo-system-decls 
							"prim declarations must be of the form: "
							"prim id value"))))

			    (((or 'sysname 'SYSNAME) name)  
			     (if (symbol? name)
				 (hash-table-set! sys (nemo-intern 'name) (SYSNAME name))
				 (nemo:error 'eval-nemo-system-decls
					     "system name must be a symbol")))
			    
			    (((or 'component 'COMPONENT)
			      ((or 'type 'TYPE) typ) 
			      ((or 'name 'NAME) name) . lst)

			     (let ((name1 (let ((x (and (hash-table-exists? 
							 sys (or (lookup-def name scope-subst) name))
							(hash-table-ref 
							 sys (or (lookup-def name scope-subst) name)))))
					    (or (and x (nemo:quantity? x)
						     (cases nemo:quantity x
							    (LABEL (v)  v)
							    (else name)))
						name))))

			       (let* ((sym   (fresh "comp"))
				      (scope (or scope sym)))
				 (match-let (((cqs scope-subst1)   (loop lst (list) scope scope-subst)))
					    (let ((comp  (COMPONENT name1 typ cqs scope-subst1)))
					      (hash-table-set! sys sym comp)
					      (list (cons sym qs) scope-subst1))))))

			    (((or 'component 'COMPONENT) ((or 'type 'TYPE) typ)  . lst)  
			     (let* ((sym   (fresh "comp"))
				    (scope (or scope sym)))

                               (match-let (((cqs scope-subst1)   (loop lst (list) scope scope-subst)))
                                          (let ((comp  (COMPONENT sym typ cqs scope-subst1)))
                                            (hash-table-set! sys sym comp)
                                            (list (cons sym qs) scope-subst1)))))

			    (((or 'component 'COMPONENT)  ((or 'name 'NAME) name) '= 
			      (and functor-name (? symbol?)) 
			      (and args (? list?)))

			     (if (and scope scope-subst) 
				 (nemo:error 'eval-nemo-system-decls
					     "functor instantiation is not permitted in nested scope"))

			     (match-let
			      (((functor-args functor-type functor-lst)
				(let ((x (hash-table-ref sys functor-name)))
				  (or (and (nemo:quantity? x)
					   (cases nemo:quantity x
						  (FUNCTOR (sym args typ lst)  (list args typ lst))
						  (else #f)))
				      (nemo:error 'eval-nemo-system-decls! functor-name 
						  " is not a functor" )))))

			      (if (not (<= (length functor-args)  (length args)))
				  (let ((n (length args)))
				    (nemo:error 'eval-nemo-system-decls! "functor " functor-name 
						" requires at least " (length functor-args) " arguments; "
						args  " (total " n ") "
						(if (= n 1) "was" "were") " given" )))


			      (match-let
			       (((cqs1 scope-subst1)   (loop args (list) name scope-subst)))
			         (let ((cqs1-names (sort (map ->string cqs1) string< ))
				       (args-names (let ((qs (map (lambda (x) 
								    (->string (compute-qid x name scope-subst1)) )
								    functor-args)))
						     (sort qs string<))))

				   (if (not (every (lambda (x) (member x cqs1-names string=)) args-names))
				       (nemo:error 'eval-nemo-system-decls! "functor " functor-name 
					      " instantiation did not include required arguments " 
					      (filter (lambda (x) (not (member x cqs1-names string=))) args-names)))
			       
				   (match-let
				    (((cqs2 scope-subst2)   (loop functor-lst (list) name scope-subst1)))
				    (let* ((sym    (fresh "comp"))
					   (comp   (COMPONENT name functor-type (append cqs1 cqs2) scope-subst2)))
				      (hash-table-set! sys sym comp)
				      
				      (list (cons sym qs) scope-subst)))))))
			     
			    ((or 
                              ((or 'functor 'FUNCTOR) ((or 'name 'NAME) name) ((or 'type 'TYPE) typ)
                               (and args (? list?))  '= . lst)
                              ((or 'functor 'FUNCTOR) ((or 'type 'TYPE) typ) ((or 'name 'NAME) name)
                               (and args (? list?))  '= . lst)
                              ((or 'functor 'FUNCTOR) ((or 'name 'NAME) name) ((or 'type 'TYPE) typ)
                               (and args (? list?))  . lst)
                              ((or 'functor 'FUNCTOR) ((or 'type 'TYPE) typ) ((or 'name 'NAME) name)
                               (and args (? list?))  . lst))

			     (if (and scope scope-subst) 
				 (nemo:error 'eval-nemo-system-decls
					     "functor declaration is not permitted in nested scope"))
			     (let* ((sym      (string->symbol (->string name)))
				    (functor  (FUNCTOR sym args typ lst)))
			       (if (hash-table-exists? sys sym)
				   (nemo:error 'eval-nemo-system-decls! ": functor " sym " already defined"))
			       (hash-table-set! sys sym functor)
			       (list (cons sym qs) scope-subst)))
			    
			    (((or 'const 'CONST) . _)
			     (nemo:error 'eval-nemo-system-decls "declaration: " decl
					 ": constant declarations must be of the form: "
					    "const id = expr"))
			    
			    ((id '= . _) 
			     (nemo:error 'eval-nemo-system-decls 
					 "declaration " decl
					 ": algebraic equations must be of the form: "
					 "id = expr")) 
			    
			    (((or 'reaction 'REACTION) . _)
			     (nemo:error 'eval-nemo-system-decls 
					 "declaration " decl 
					 ": reaction declarations must be of the form: "
					 "reaction (id ...)"))
			    
			    (((or 'fun 'FUN 'rel 'REL 'defun 'DEFUN) . _) 
			     (nemo:error 'eval-nemo-system-decls "function declarations must be of the form: "
					    "fun id (arg1 arg2 ...) expr"))
			    
			    (((or 'prim 'PRIM) . _) 
			     (nemo:error 'eval-nemo-system-decls "prim declarations must be of the form: "
					    "prim id value"))
			    
			    (((or 'component 'COMPONENT) . _)  
			     (nemo:error 'eval-nemo-system-decls "invalid component: " decl))
			    
			    (((or 'sysname 'SYSNAME) . _)  
			     (nemo:error 'eval-nemo-system-decls "system name must be of the form (sysname name)"))
			    
			    ;; anything that doesn't match is possibly
			    ;; declarations recognized by the nemo extension
			    ;; modules
			    (((and tag (? symbol?))  . lst)
			     (match-let (((typ name alst)  
					  (let loop ((lst lst) (ax (list tag)))
					    (if (null? lst)
						(list (list (car (reverse ax))) #f (cdr (reverse ax)))
						(match lst
						       (((? symbol?) . rest) 
							(loop (cdr lst) (cons (car lst) ax) ))
						       (((x . rest)) 
							  (if (and (symbol? x) (every list? rest))
							      (list (reverse ax) x rest)
							      (list (reverse ax) #f lst)))
						       (else  (list (reverse ax) #f lst)))))))
					
					(let* ((name (or name (fresh tag)))
					       (qid  name))
					  (env-extend! qid  typ (if scope (append alst `((scope ,scope))) alst))
					  (list (cons qid qs) (update-subst name qid scope-subst)))))

			    (else
			     (nemo:error 'eval-nemo-system-decls 
					 "declaration " decl ": "
					 "extended declarations must be of the form: "
					 "declaration (name (properties ...)"
					 ))
			    )))
                   (loop (cdr ds) qs1 scope scope-subst1))
                  ))
                ))
          ))
     res
     ))

)
