;;       
;; An extension for translating NEMO models to Octave Oct-files.
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

(module nemo-octavecc

	(nemo:octavecc-translator)

	(import scheme chicken utils data-structures lolevel ports extras srfi-1 srfi-13 srfi-69)
	
	(require-extension lolevel matchable strictly-pretty 
			   varsubst datatype nemo-core nemo-utils
			   nemo-gate-complex nemo-synapse)

(define oct-builtin-consts  `())

(define C-ops
  `(+ - * / > < <= >= =))

(define builtin-fns
  `(+ - * / pow neg abs atan asin acos sin cos exp ln
      sqrt tan cosh sinh tanh hypot gamma lgamma log10 log2 log1p ldexp cube
      > < <= >= = and or round ceiling floor max min
      ))

(define (oct-name s)
  (let ((cs (string->list (->string s))))
    (let loop ((lst (list)) (cs cs))
      (if (null? cs) (string->symbol (list->string (reverse lst)))
	  (let* ((c (car cs))
		 (c1 (cond ((or (char-alphabetic? c) (char-numeric? c) (char=? c #\_)) c)
			   (else #\_))))
	    (loop (cons c1 lst) (cdr cs)))))))
	    
(define (rhsexpr/C++ expr)
  (match expr 
	 (('if . es)  `(if . ,(map (lambda (x) (rhsexpr/C++ x)) es)))
	 (('pow x y)  (if (and (integer? y)  (positive? y))
			  (if (> y 1)  (let ((tmp (gensym "x")))
					 `(let ((,tmp ,x)) (* . ,(list-tabulate (inexact->exact y) (lambda (i) tmp)))))
			      x)
			    (if (and (number? y) (zero? y)) 1.0 expr)))
	 ((s . es)    (if (symbol? s)  (cons (if (member s builtin-fns) s (oct-name s))
					     (map (lambda (x) (rhsexpr/C++ x)) es)) expr))
	 (id          (if (symbol? id) (oct-name id) id))))

(define (oct-state-name n s)
  (oct-name (s+ n s)))

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
	  (cons (if (member f builtin-fns) f (oct-name f)) (map name-normalize es)))
	 ((? symbol? ) (oct-name expr))
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
		(if (member op C-ops)
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




(define (make-define-fn sysname )
  (lambda (indent n proc)

    (let ((lst (procedure-data proc))
	  (indent+ (+ 2 indent)))

      (let ((rt       (or (lookup-def 'rt lst) 'double))
	    (formals  (lookup-def 'formals lst))
	    (vars     (lookup-def 'vars lst))
	    (consts   (filter (lambda (x) (not (procedure? (cdr x)))) (lookup-def 'consts lst)))
	    (body     (lookup-def 'body lst))
	    (rv       (gensym 'rv)))

	(let ((argument-list 
	       (append
		(if (null? vars) '() (map (lambda (x) (s+ "double " (oct-name x))) vars))
		'("const void* params"))))
	  (pp indent ,nl (,rt ,(oct-name n) (,(slp ", " argument-list)) "{" ))
	  (let* ((body0 (rhsexpr/C++ body))
		 (body1 (canonicalize-expr/C++ (add-params-to-fncall body0)))
		 (lbs   (enum-bnds body1 (list))))
	    (pp indent+ (double ,rv ";"))
	    (if (not (null? lbs)) (pp indent+ (double ,(slp ", " lbs) ";")))
	    (if (not (null? consts)) 
		(begin (pp indent+ (double ,(slp ", " (map (compose oct-name car) consts)) ";")
			   ("const " ,(s+ sysname "::Parameters_") "& p =  *(reinterpret_cast< const " ,(s+ sysname "::Parameters_") "*>(params));"))
		       (for-each (lambda (x) (let ((n (car x))) (pp indent+ (,(oct-name n) = ,(s+ "p." (oct-name n) ";"))))) consts)
		       ))
	    (pp indent+ ,(expr->string/C++ body1 (oct-name rv)))
	    (pp indent+ ,(s+ "return " rv ";"))
	    ))
	(pp indent "}"))
    )))


(define (make-define-fn-header sysname )
  (lambda (indent n proc)

    (let ((lst (procedure-data proc))
	  (indent+ (+ 2 indent)))

      (let ((rt       (or (lookup-def 'rt lst) 'double))
	    (formals  (lookup-def 'formals lst))
	    (vars     (lookup-def 'vars lst))
	    (consts   (filter (lambda (x) (not (procedure? (cdr x)))) (lookup-def 'consts lst)))
	    (body     (lookup-def 'body lst))
	    (rv       (gensym 'rv)))

	(let ((argument-list 
	       (append
		(if (null? vars) '() (map (lambda (x) (s+ "double " (oct-name x))) vars))
		'("const void* params"))))
	  (pp indent ,nl (,rt ,(oct-name n) (,(slp ", " argument-list)) ";" ))
	  ))
      )))



(define (ith v i) (sprintf "Ith(~A,~A)" v i))


(define (output-dy sysname method imports const-defs state-index-map 
		   external-eq-defs rate-eq-defs reaction-eq-defs asgn-eq-defs
		   pool-ions mcap i-eqs v-eq indent indent+)


  (pp indent ,nl (
		  ,(s+ "int " sysname "_dynamics")
		  (,(slp ", " `("double t"
				"const double y[]"
				"double f[]"
				"void* sys"
				)))
		     #\{
		     ))

  (let* (
	 (asgn-eq-defs 
	  (map 
	   (lambda (def) (list (first def) 
			       (add-params-to-fncall 
				(canonicalize-expr/C++ (second def)))))
	   asgn-eq-defs))

	 (rate-eq-defs 
	  (map 
	   (lambda (def) (list (first def) 
			       (add-params-to-fncall 
				(canonicalize-expr/C++ (second def)))))
	   rate-eq-defs))

	 (reaction-eq-defs 
	  (map 
	   (lambda (def) (list (first def) 
			       (add-params-to-fncall 
				(canonicalize-expr/C++ (second def)))))
	   reaction-eq-defs))

	 (i-eqs 
	  (map 
	   (lambda (def) (list (first def) 
			       (add-params-to-fncall (canonicalize-expr/C++ (second def)))))
	   i-eqs))

	 (v-eq
	  (and v-eq
	       (list (first v-eq) 
		     (add-params-to-fncall (canonicalize-expr/C++ (second v-eq))))))

	 (eqs 
	  (append 
	   
	   asgn-eq-defs
	   reaction-eq-defs
		 
	   (map (lambda (pool-ion)
		  (let ((n (third pool-ion))
			(b (first pool-ion)))
		    (list n b)))
		pool-ions)))
	 
	 (eq-dag 
	  (map (lambda (def)
		 (cons (first def) (enum-freevars (second def) '() '())))
	       eqs))

	 (eq-order
	  (reverse
	   (topological-sort eq-dag 
			     (lambda (x y) (string=? (->string x) (->string y))))))

	 (eq-locals  (find-locals 
		      (map second
			   (if v-eq (cons v-eq (append i-eqs rate-eq-defs eqs))
			       (append i-eqs rate-eq-defs eqs)))))
	 )


    (pp indent+ (double ,(slp ", " (delete-duplicates 
				    (map (compose ->string oct-name)
					 (filter (lambda (x) (not (member x oct-builtin-consts)))
						 (append 
						  eq-locals
						  eq-order
						  (map first imports)
						  (map second imports)
						  (map first i-eqs)
						  (map first state-index-map)
						  (map first const-defs)
						  )))
				    string=?))
			";"))

    (pp indent+ ,nl
	
	("// cast the node ptr to an object of the proper type")
	("assert(pnode);")
	("const " ,sysname "& pnode =  *(reinterpret_cast<" ,sysname "*>(sys));")
	(,sysname "& snode =  *(reinterpret_cast<" ,sysname "*>(sys));")

	,nl

	("// params is a reference to the model parameters ")
	(const struct ,(s+ sysname "_Parameters_") "*params;")
	("params = &(pnode.P_);")
	
	,nl

	("// state is a reference to the model state ")
	(struct ,(s+ sysname "_State_") "*state;")
	("state = &(snode.S_);")
	
	,nl
	)

    (for-each (lambda (n) (pp indent+ ,(expr->string/C++ (sprintf "state->~A" n) n)))
	      (map (compose oct-name second) imports))
    
    (for-each (lambda (def)
		(let ((n (first def))
		      (b (second def)))
		  (pp indent+ ,(expr->string/C++ b (oct-name n)))))
	      external-eq-defs)
    
    (for-each (lambda (def)
		(let ((n (first def)) )
		  (pp indent+
		      ,(expr->string/C++ 
			(s+ "params->" n) n))))
	      const-defs)

    (let ((vi (lookup-def 'v state-index-map)))
      (if vi (pp indent+ ,(expr->string/C++ (ith 'y vi) 'v))))

    (for-each (lambda (def)
		(let* ((n (first def))
		       (ni (lookup-def n state-index-map)))
		  (pp indent+ ,(expr->string/C++ (ith 'y ni) (oct-name n)))))
	      rate-eq-defs)

    (for-each (lambda (n)
		(let ((b  (lookup-def n eqs)))
		  (if b (pp indent+ ,(expr->string/C++ b (oct-name n))))))
	      eq-order)

    (for-each (lambda (def)
		(let* ((n (first def))
		       (b (second def))
		       (fv (ith 'f (lookup-def n state-index-map)))
		       )
		  (pp indent+ ,(s+ "// rate equation " n)
		      ,(expr->string/C++ b fv))))
	      rate-eq-defs)
    
    (for-each (lambda (def) 
		(pp indent+ ,(expr->string/C++ (second def) (first def)))) 
	    i-eqs)
    
    (if v-eq
	(let ((vi (lookup-def 'v state-index-map)))
	  (if vi
	      (pp indent+ ,(expr->string/C++ (second v-eq) (ith 'f vi)))
	      )))

    (for-each (lambda (n) (pp indent+ ,(expr->string/C++ n (sprintf "state->~A" n))))
	      (map (compose oct-name second) imports))
    

    (pp indent+ ,nl ("return 0;"))

    (pp indent  #\})

    ))



(define (output-parameters sysname imports const-defs indent indent+)

  (let* ((parameter-defs
	  (map 
	   (lambda (def) (list (first def) 
			       (add-params-to-fncall (canonicalize-expr/C++ (second def)))))
	   const-defs))
	 (parameter-locals  (find-locals (map second parameter-defs))))

    (pp indent ,nl (,(s+ sysname "_Parameters_::Parameters_") () ":"))

    (let* ((const-exprs
	    (map (lambda (def)
		   (let* ((n  (first def)) (b (second def)))
		     (s+ (oct-name n) "  (" (expr->string/C++ b) ")")))
		 const-defs) )
	   (import-qs (delete-duplicates (map (compose oct-name second) imports)))
	   (import-exprs (map (lambda (n) (s+ n "  (0.0)")) import-qs) )
	   )
      
      (if (not (null? parameter-locals)) 
	  (pp indent+ (double ,(slp ", " parameter-locals) ";")))

      (pp indent+ ,(slp ",\n" (cons "I_scale (1.0)" (append import-exprs const-exprs))))
      
      (pp indent ("{}"))
      )

    (pp indent ,nl (,(s+ sysname "_Parameters_::Parameters_") (double I_scale_val) ":"))

    (let* ((const-exprs
	    (map (lambda (def)
		   (let* ((n  (first def)) (b (second def)))
		     (s+ (oct-name n) "  (" (expr->string/C++ b) ")")))
		 const-defs) )
	   (import-qs (delete-duplicates (map (compose oct-name second) imports)))
	   (import-exprs (map (lambda (n) (s+ n "  (0.0)")) import-qs) )
	   )
      
      (if (not (null? parameter-locals)) 
	  (pp indent+ (double ,(slp ", " parameter-locals) ";")))

      (pp indent+ ,(slp ",\n" (cons "I_scale (I_scale_val)" (append import-exprs const-exprs))))

      
      (pp indent ("{}"))
      )

    (pp indent ,nl (,(s+ sysname "_Parameters_::Parameters_") 
		    (,(slp ", " (cons "double I_scale_val" 
				      (map (lambda (n) (s+ "double " n "_val")) import-qs)))
		     ":")))
    
    (let* ((const-exprs
	    (map (lambda (def)
		   (let* ((n  (first def)) (b (second def)))
		     (s+ (oct-name n) "  (" (expr->string/C++ b) ")")))
		 const-defs) )
	   (import-qs (delete-duplicates (map (compose oct-name second) imports)))
	   (import-exprs (map (lambda (n) (s+ n "  (" (s+ n "_val") ")")) import-qs) )
	   )
      
      (if (not (null? parameter-locals)) 
	  (pp indent+ (double ,(slp ", " parameter-locals) ";")))
      
      (pp indent+ ,(slp ",\n" (cons "I_scale (I_scale_val)" (append import-exprs const-exprs))))
	
      (pp indent ("{}")))

  ))


(define (output-init sysname state-index-map steady-state-index-map 
		     imports external-eq-defs const-defs asgn-eq-defs init-eq-defs rate-eq-defs 
		     reaction-eq-defs i-eqs v-eq pool-ions perm-ions indent indent+)

  
  (let* ((N (length state-index-map))

	 (asgn-eq-defs
	  (map 
	   (lambda (def) (list (first def) 
			       (add-params-to-fncall 
				(canonicalize-expr/C++ (second def)))))
	   asgn-eq-defs))
	 
	 (init-eq-defs
	  (map 
	   (lambda (def) (list (first def) 
			       (add-params-to-fncall 
				(canonicalize-expr/C++ (second def)))))
	   init-eq-defs))

	 (reaction-eq-defs 
	  (map 
	   (lambda (def) (list (first def) 
			       (add-params-to-fncall 
				(canonicalize-expr/C++ (second def)))))
	   reaction-eq-defs))

	 (i-eqs 
	  (map 
	   (lambda (def) (list (first def) 
			       (add-params-to-fncall 
				(canonicalize-expr/C++ (second def)))))
	   i-eqs))

	 (v-eq
	  (and v-eq
	       (list (first v-eq) 
		     (add-params-to-fncall 
		      (canonicalize-expr/C++ (second v-eq))))))
	 
	 (init-eqs 
	  (append 
	   
	   asgn-eq-defs
	   init-eq-defs

	   (map (lambda (pool-ion)
		  (let ((n (third pool-ion))
			(b (first pool-ion)))
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

	 (init-locals  (find-locals (map second (append init-eqs i-eqs reaction-eq-defs))))

	 )
    
    (pp indent ,nl (,(s+ sysname "_State_") (const ,(s+ sysname "_Parameters_") & p)))
    
    (pp indent  #\{)

    (pp indent+
	(double ,(slp ", " (delete-duplicates
			    (map ->string 
				 (filter (lambda (x) (and (not (member x oct-builtin-consts))
							  (not (member x (map second imports)))))
					 (append 
					  init-locals
					  init-order
					  (map first external-eq-defs)
					  (map first i-eqs)
					  (map first state-index-map) 
					  (map first const-defs)
					  (map first reaction-eq-defs)
					  )))
				 string=?))
		      ";")
	(,(s+ "const " sysname "_Parameters_ *params = &p;"))
	)

    (pp indent+ ,(sprintf "memset(y_,0,~A*sizeof(double));" N))

    (for-each
     (lambda (n) (pp indent+ ,(expr->string/C++ (s+ "p." n) n)))
     (append (delete-duplicates (map (compose oct-name second) imports))
	     (map (compose oct-name first) const-defs) ))

    (let ((vi (lookup-def 'v state-index-map))
	  (vrest (or (and (lookup-def 'Vrest const-defs) 'Vrest) -65.0)))
      (if (and vi vrest) 
	  (pp indent+ ,(expr->string/C++  vrest 'v ))))

    (for-each (lambda (def)
		(let ((n (first def))
		      (b (second def)))
		  (pp indent+ ,(expr->string/C++ b (oct-name n)))))
	      external-eq-defs)

    (for-each (lambda (n)
		(let ((b  (lookup-def n init-eqs)))
		  (if b (pp indent+ ,(expr->string/C++ b (oct-name n))))))
	      init-order)
    
    (for-each (lambda (def)
		(let* ((n  (first def)) 
		       (ni (lookup-def n state-index-map)))
		  (if ni (pp indent+ ,(expr->string/C++ n  (s+ "y_[" ni "]"))))))
	      init-eq-defs)

    (for-each
     (lambda (def)
       (let ((n (first def)) (b (second def)))
	 (if (not (lookup-def n init-eq-defs))
	     (pp indent+ ,(expr->string/C++ b n)))))
     reaction-eq-defs)
    
    (for-each
     (lambda (def) 
       (pp indent+ ,(expr->string/C++ (second def) (first def))))
     i-eqs)

    (let ((vi (lookup-def 'v state-index-map)))
      (if vi (pp indent+ ,(expr->string/C++ 'v (s+ "y_[" vi "]")))))
      
    (pp indent  #\})
    
    (pp indent ,nl
	(,(s+ sysname "_State_") (const State_& s) ": " )
	(,(slp ",\n" (map (lambda (n) (sprintf "~A(s.~A)" n n))
			  (cons "r_"
				(delete-duplicates
				 (append (map (compose oct-name second) imports)
					 (map (compose oct-name third) pool-ions)
					 ))
				))
	       ))
	)

    (pp indent  #\{)
    (pp indent+ (,(sprintf "for ( int i = 0 ; i < ~A ; ++i ) y_[i] = s.y_[i];" N)))

    (pp indent  #\})

    (pp indent ,nl (,(s+ sysname "::State_& " sysname "::State_::operator=(const State_& s)")))

    (pp indent  #\{)


    (pp indent+ (,(sprintf #<<EOF
   assert(this != &s);  
     for ( size_t i = 0 ; i < ~A ; ++i )
       y_[i] = s.y_[i];
EOF
N)))
    (pp indent+
	(,(slp "\n" (map (lambda (n) (expr->string/C++ (sprintf "s.~A" n) n))
			  (cons "r_"
				(delete-duplicates
				 (append (map (compose oct-name second) imports)
					 (map (compose oct-name third) pool-ions)
					 ))
				))
	       ))
	)

     (pp indent+ "return *this;")

     (pp indent  #\})

)))




(define (output-prelude sysname steady-state-index-map indent)
  (pp indent (,(sprintf "#include \"~A.h\""  sysname)
	      ,(sprintf "#include \"octave/oct.h\"")
	      ))
  )

(define (output-header 
	 sysname method state-index-map steady-state-index-map 
	 imports const-defs asgn-eq-defs init-eq-defs rate-eq-defs 
	 reaction-eq-defs i-eqs pool-ions perm-ions 
	 synapse-info
	 indent indent+)

  (pp indent
      ("#define Ith(v,i)    (v[i])   /* Ith component in a vector */")
      ("typedef int (*rhsfn_t)(double t, const double *y, double *ydot, void *user_data);")
      ,nl)

  
  (pp indent (int ,(s+ sysname "_dynamics")\
		  "(double, const double *, double *, void*)" #\;) ,nl)
  
  (pp indent+
      (,(s+ "struct " sysname "_Parameters_ { ")
       ("double_t I_scale; /* scaling factor for model currents */")))

  (for-each 
   (lambda (x) (pp indent+ (,(sprintf "double ~A;" x))))
   (delete-duplicates (map (compose oct-name second) imports)))

  (for-each 
   (lambda (x) (pp indent+ (,(sprintf "double ~A;" x))))
   (map (compose oct-name first) const-defs))

  (pp indent+ 
      ("Parameters_(); // default parameters constructor: I_scale set to 1 and imported quantities set to 0")
      ("Parameters_(double); // parameters constructor with given I_scale")
      ("Parameters_(" 
       (slp ", " 
	    (cons "double" 
		  (map (lambda (x) "double") 
		       (delete-duplicates (map (compose oct-name second) imports)))
		  ))
       "); // parameters constructor with given I_scale and imported quantities")
      )

  (pp indent+ "};")

  (pp indent+ (,(s+ "struct " sysname "_State_ { ")))

  (pp indent+ ,nl 

      (,(sprintf "double y_[~A];" (length state-index-map)))

      ,(if (not (and (null? imports) (null? pool-ions)))
	  `(,(sprintf "double ~A;" 
		      (slp ", " 
			   (delete-duplicates
			    (append (map (compose oct-name second) imports)
				    (map (compose oct-name third) pool-ions)
				    ))
			   ))
	    )
	  `())

      "State_(const Parameters_& p);" 
      "State_(const State_& s);"
      "State_& operator=(const State_& s);"
      )

  (pp indent+ "};")

  )


(define (output-dld sysname)

    ,(sprintf
#<<EOF

DEFUN_DLD (~A_initial, args, nargout, "~A initial values")
{
     int nargin = args.length ();
     double *Avec;
     const char *libargs = "";
     octave_value_list retval;

     dim_vector Adv (2);
     Adv(0) = ~A; Adv(1) = 1;

     dim_vector Bdv (2);
     Bdv(0) = ~A; Bdv(1) = 1;

     NDArray A (Adv);
     NDArray B (Bdv);
     
     Avec = A.fortran_vec();
     Bvec = B.fortran_vec();

     ~A_clib_initial ((void *)Avec);
     ~A_clib_parameters ((void *)Bvec);

     nargout = 2;
     
     retval(0) = octave_value(A);
     retval(1) = octave_value(B);

     return retval;
}

EOF
ivp-id N P ivp-id ivp-id)


,(sprintf 
#<<EOF

DEFUN_DLD (~A_dynamics, args, , "~A")
{
     int nargin = args.length ();
     double *Fvec; const double *Yvec;
     const char *libargs = "";
     unsigned int N;

     if (nargin != 3)
     {
	  print_usage ();
	  return octave_value_list ();
     }
     else
     {
	  const NDArray T = args(0).array_value();
	  const NDArray Y = args(1).array_value();
	  const void *U = args(2).pointer_value();


	  dim_vector Ydims = Y.dims();

	  N = Ydims(0);
	  const NDArray F(N);

          Yvec = Y.fortran_vec();
          Fvec = F.fortran_vec();
	  ~A_dynamics (T[0], Yvec, Fvec, U);

	  return octave_value_list(Fvec);
     }
}

EOF
sysname sysname sysname)
)
  


(define (nemo:octavecc-translator sys . rest)

  (define (cid x)  (second x))
  (define (cn x)   (first x))

  (let-optionals rest ((dirname ".") (method #f))

    (let ((method (or method 'gsl)))

      (if (not (member method '(lsode cvode leapfrog)))
	  (nemo:error 'nemo:octavecc-translator ": unknown method " method))

  (match-let ((($ nemo:quantity 'DISPATCH  dis) (hash-table-ref sys (nemo-intern 'dispatch))))
    (let ((imports  ((dis 'imports)  sys))
	  (exports  ((dis 'exports)  sys)))
      (let* ((indent      0)
	     (indent+     (+ 2 indent ))

	     (sysname     (oct-name ((dis 'sysname) sys)))
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
				  (and (not (member (first nv) oct-builtin-consts))
				       (let ((v1 (canonicalize-expr/C++ (second nv))))
					 (list (oct-name (first nv)) v1))))
				consts))
	     
	     (gate-complex-info    (nemo:gate-complex-query sys))
	     (synapse-info         (nemo:post-synaptic-conductance-query sys))
	     (i-synapses           (lookup-def 'i-synapses synapse-info))

	     (gate-complexes  (lookup-def 'gate-complexes gate-complex-info))
	     (perm-ions       (map (match-lambda ((comp i e erev val) `(,comp ,(oct-name i) ,(oct-name e) ,erev)))
				   (lookup-def 'perm-ions gate-complex-info)))
	     (acc-ions        (map (match-lambda ((comp i in out) `(,comp ,@(map oct-name (list i in out)))))
				   (lookup-def 'acc-ions gate-complex-info)))
	     (epools          (lookup-def 'pool-ions gate-complex-info))
	     (pool-ions       (map (lambda (lst) (map oct-name lst)) epools))

	     (i-gates         (lookup-def 'i-gates gate-complex-info))

	     (capcomp         (any (match-lambda ((name 'membrane-capacitance id) (list name id)) (else #f)) components))
	     (mcap            (and capcomp (car ((dis 'component-exports) sys (cid capcomp)))))
		
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
			      (sts               (map (lambda (gate) ((dis 'component-exports) sys (cid gate))) gates))
			      )


                         (if (and pore (null? permqs))
                             (nemo:error 'nemo:octavecc-translator ": ion channel definition " label
                                         "permeating-ion component lacks exported quantities"))

			 (for-each 
			  (lambda (st)
			    (if (null? st)
				(nemo:error 'nemo:octavecc-translator: "ion channel definition " label
					    "gate component lacks exported quantities")))
			  sts)
			 
			 (if (not (or pore permeability))
			     (nemo:error 'nemo:octavecc-translator ": ion channel definition " label
					 "lacks any pore or permeability components"))


			 (cond ((and perm permeability (pair? gates))
				     (let* ((i     (oct-name (s+ 'i (cn perm))))
					    (pmax  (car ((dis 'component-exports) sys (cid permeability))))
					    (pwrs  (map (lambda (st) (map (lambda (n) (state-power sys n)) st)) sts))
					    (gpwrs (map (lambda (st pwr) (map (lambda (s p) (if p `(pow ,s ,p) s)) st pwr)) sts pwrs))
					    (gion  `(* ,pmax ,(sum (map (lambda (gpwr) 
									  (match gpwr ((x)  x) (else `(* ,@gpwr))))
									gpwrs))))
					    )
					 (list i #f gion (oct-name (s+ 'i_ label) ))))

			       ((and perm pore (pair? gates))
				(case (cn perm)
				  ((non-specific)
				   (let* ((i     (oct-name 'i))
					  (e     (car permqs))
					  (gmax  (car ((dis 'component-exports) sys (cid pore))))
					  (pwrs  (map (lambda (st) (map (lambda (n) (state-power sys n)) st)) sts))
					  (gpwrs (map (lambda (st pwr) (map (lambda (s p) (if p `(pow ,s ,p) s)) st pwr)) sts pwrs))
					  (gion  `(* ,gmax ,(sum (map (lambda (gpwr) 
									(match gpwr ((x)  x) (else `(* ,@gpwr))))
								      gpwrs))))
					  )
				     (list i e gion  (oct-name (s+ 'i_ label) ))))

				  (else
				   (let* ((i     (oct-name (s+ 'i (cn perm))))
					  (e     (car permqs))
					  (gmax  (car ((dis 'component-exports) sys (cid pore))))
					  (pwrs  (map (lambda (st) (map (lambda (n) (state-power sys n)) st)) sts))
					  (gpwrs (map (lambda (st pwr) (map (lambda (s p) (if p `(pow ,s ,p) s)) st pwr)) sts pwrs))
					  (gion  `(* ,gmax ,(sum (map (lambda (gpwr) 
									(match gpwr ((x)  x) (else `(* ,@gpwr))))
								      gpwrs))))
					  )
				     (list i e gion (oct-name (s+ 'i_ label) ))))))
			       
			       ((and perm pore)
				(case (cn perm)
				  ((non-specific)
				   (let* ((i     (oct-name 'i))
					  (e     (car permqs))
					  (gmax  (car ((dis 'component-exports) sys (cid pore)))))
				     (list i e gmax (oct-name (s+ 'i_ label) ))))
				  (else
				   (nemo:error 'nemo:octavecc-translator ": invalid ion channel definition " label))))
			       
			       ((and acc pore (pair? gates))
				(let* ((i     (oct-name (s+ 'i (cn acc))))
				       (gmax  (car ((dis 'component-exports) sys (cid pore))))
				       (pwrs  (map (lambda (st) (map (lambda (n) (state-power sys n)) st)) sts))
				       (gpwrs (map (lambda (st pwr) (map (lambda (s p) (if p `(pow ,s ,p) s)) st pwr)) sts pwrs))
				       (gion  `(* ,gmax ,(sum (map (lambda (gpwr) 
								     (match gpwr ((x)  x) (else `(* ,@gpwr))))
								   gpwrs))))
				       )
				  (list i #f gion  (oct-name (s+ 'i_ label) ))))
			       (else (nemo:error 'nemo:octavecc-translator ": invalid ion channel definition " label))
			       )))
		     gate-complexes))

	     (i-names (delete-duplicates (map first i-eqs)))
		
	     (i-eqs  (fold  (lambda (i-gate ax) 
			      (let ((i-gate-var (first i-gate)))
				(cons (list (oct-name 'i) #f i-gate-var (s+ 'i_ (second i-gate))) ax)))
			    i-eqs i-gates))

	     (i-eqs  (fold  (lambda (i-synapse ax) 
			      (cons (list (first i-synapse) (third i-synapse) (fourth i-synapse) (gensym 'i_syn )) ax))
			    i-eqs 
			    i-synapses))

	     (i-bkts (bucket-partition (lambda (x y) (eq? (car x) (car y))) i-eqs))
	     
	     (i-eqs  (fold (lambda (b ax) 
			     (match b 
				    ((and ps ((i e gion ii) . rst))  
				     (let loop ((ps ps) (summands (list)) (eqs (list)))
				       (if (null? ps)
					   
					   (let* ((sum0  (sum summands))
						  (sum1  (rhsexpr/C++ sum0))
						  (sum2  (add-params-to-fncall 
							  (canonicalize-expr/C++ sum1))))
					     (append eqs (list (list i sum2)) ax))
					   
					   (match-let (((i e gion ii) (car ps)))
						      (loop (cdr ps) 
							    (cons ii summands) 
							    (let* ((expr0 (rhsexpr/C++ (if e `(* ,gion (- v ,e)) gion)))
								   (expr1 (canonicalize-expr/C++ expr0)))
							      (cons (list ii expr1) eqs)))))))
				    
				    ((i e gion ii)
				     (let* ((expr0  (rhsexpr/C++ (if e `(* ,gion (- v ,e)) gion)))
					    (expr1  (canonicalize-expr/C++ expr0)))
				       (cons (list i expr1) ax)))
				    
				    (else ax)))
			   (list) i-bkts))

	     (external-eq-defs   (sys->external-eq-defs sys))

	     (asgn-eq-defs     (poset->asgn-eq-defs poset sys))
	     
	     (rate-eq-defs       (reverse (poset->rate-eq-defs poset sys)))
	     
	     (reaction-eq-defs   (poset->reaction-eq-defs poset sys))
	     
	     (init-eq-defs       (poset->init-defs poset sys))
	     
	     (conserve-eq-defs   (map (lambda (eq) (list 0 `(- ,(second eq) ,(first eq)))) 
				      (poset->state-conserve-eq-defs poset sys)))
	     
	     (globals          (map oct-name 
				    (filter
				     (lambda (x) (not (member x oct-builtin-consts)))
				     (delete-duplicates (append 
							exports
							(map first   imports) 
							(map second  imports) 
							(map second  perm-ions)
							(map third   perm-ions)
							(map second  acc-ions)
							(map third   acc-ions)
							(map fourth  acc-ions)
							(map second  pool-ions)
							(map third   pool-ions)
							(map first   const-defs)
							(map first   asgn-eq-defs)
							(map (lambda (gate-complex) (oct-name (s+ 'i_ (first gate-complex)))) gate-complexes )
							(map (lambda (i-gate) (oct-name (s+ 'i_ (second i-gate)))) i-gates )
							)))
				    ))

	     (imports-sans-v (filter (lambda (x) (not (equal? 'v (first x)))) imports))

	     (v-eq    (if (and mcap (member 'v globals))
			  (list 'v (rhsexpr/C++ `(+ "(node.B_.I_stim_)"  
						    (/ (* (neg ,(sum i-names)) "node.P_.I_scale") ,mcap))))
			  (list 'v 0.0)))
	     
	     (v-init-eq  (if (and mcap (member 'v globals))
			     (list 'v (rhsexpr/C++ `(/ (* (neg ,(sum i-names)) "p.I_scale") ,mcap)))
			     (list 'v 0.0)))
	     
	     (state-index-map  (let ((acc (fold (lambda (def ax)
						  (let ((st-name (first def)))
						    (list (+ 1 (first ax)) 
							  (cons `(,st-name ,(first ax)) (second ax)))))
						(list 0 (list)) 
						(if (member 'v globals)
						    (cons (list 'v) rate-eq-defs)
						    rate-eq-defs)
						)))
				 (second acc)))
	     
	     (steady-state-index-map  (let ((acc (fold (lambda (def ax)
							 (let ((st-name (first def)))
							   (if (not (alist-ref st-name init-eq-defs))
							       (list (+ 1 (first ax)) 
								     (cons `(,st-name ,(first ax)) (second ax)))
							       ax)))
						       (list 0 (list)) 
						       rate-eq-defs)))
					(second acc)))
	     
	     (dfenv 
	      (map (lambda (x) (let ((n (first x))) (list n (oct-name (s+ "d_" n )))))
		   defuns))

	     )
	
	
	
	(for-each 
	 (lambda (a)
	   (let ((acc-ion   (car a)))
	     (if (assoc acc-ion perm-ions)
		 (nemo:error 'nemo:octavecc-translator 
			     ": ion species " acc-ion " cannot be declared as both accumulating and permeating"))))
	 acc-ions)

	(for-each 
	 (lambda (p)
	   (let ((pool-ion  (car p)))
	     (if (assoc pool-ion perm-ions)
		 (nemo:error 'nemo:octavecc-translator 
			     ": ion species " pool-ion " cannot be declared as both pool and permeating"))))
	 pool-ions)

	(let ((cpp-output  (open-output-file (make-output-fname dirname prefix ".cpp")))
	      (hpp-output  (open-output-file (make-output-fname dirname prefix ".h"))))
	  
	  ;; include files and other prelude definitions
	  (with-output-to-port cpp-output
	    (lambda ()
	      (output-prelude sysname steady-state-index-map indent)
	      ))
	  
	  ;; user-defined functions
	  (let ((define-fn  (make-define-fn sysname))
		(define-fn-header (make-define-fn-header sysname)))
	    
	    (for-each (lambda (fndef) 
			(if (not (member (car fndef) builtin-fns))
			    (with-output-to-port cpp-output
			      (lambda ()
				(apply define-fn-header (cons indent fndef))
				(pp indent ,nl)))
			    ))
		      defuns)

	    (for-each (lambda (fndef) 
			(if (not (member (car fndef) builtin-fns))
			    (with-output-to-port cpp-output
			      (lambda ()
				(apply define-fn (cons indent fndef))
				(pp indent ,nl)))
			    ))
		      defuns)
	    )
	  
	  
	  ;; derivative function
	  (with-output-to-port cpp-output
	    (lambda ()
	      (output-dy sysname method imports-sans-v const-defs state-index-map 
			 external-eq-defs rate-eq-defs reaction-eq-defs asgn-eq-defs
			 pool-ions mcap i-eqs v-eq
			 indent indent+)
	      ))
	  
	  ;; system parameters
	  (with-output-to-port cpp-output
	    (lambda ()
	      (output-parameters sysname imports-sans-v const-defs 
				 indent indent+)
	      ))
	  
	  ;; initial values function
	  (with-output-to-port cpp-output
	    (lambda ()
	      (output-init sysname state-index-map steady-state-index-map 
			   imports-sans-v external-eq-defs const-defs asgn-eq-defs init-eq-defs rate-eq-defs 
			   reaction-eq-defs i-eqs v-init-eq
			   pool-ions perm-ions indent indent+)
	      (pp indent ,nl)
	      ))
	  
	  (with-output-to-port hpp-output
	    (lambda ()
	      (output-header
	       sysname method state-index-map steady-state-index-map 
	       imports-sans-v const-defs asgn-eq-defs init-eq-defs rate-eq-defs 
	       reaction-eq-defs i-eqs pool-ions perm-ions 
	       synapse-info
	       indent indent+)
	      (pp indent ,nl)
	      ))
	  
	  (close-output-port cpp-output)
	  (close-output-port hpp-output)
	  
	  ))
      ))
  ))
  )
)
