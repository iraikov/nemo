;;       
;; 
;; Utility procedures for NEMO code generators. 
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

(module nemo-utils
	
 (lookup-def alist-update bucket-partition
  enum-bnds enum-freevars find-locals sum 
  if-convert let-enum let-elim let-lift
  add-params-to-fncall
  transitions-graph state-conseqs
  state-power quantity-unit

  sys->external-eq-defs

  poset->asgn-eq-defs
  poset->rate-eq-defs
  poset->transient-event-defs
  poset->reaction-eq-defs
  poset->init-defs
  poset->state-conserve-eq-defs

  poset->asgn-eq-defs*
  poset->rate-eq-defs*
  poset->transient-event-defs*
  poset->reaction-eq-defs*
  poset->init-defs*

  template-dir instantiate-template instantiate-template* 
  read-template tenv-enter

  make-output-fname
  s+ sw+ slp nl spaces ppf
  )

 (import scheme chicken data-structures files srfi-1 srfi-13 srfi-69)

 (require-extension matchable strictly-pretty datatype
		    varsubst digraph nemo-core nemo-ersatz-lexer)
 
 (require-library ersatz-lib)

 (import (prefix ersatz-lib ersatz: ))


(define (lookup-def k lst . rest)
  (let-optionals rest ((default #f))
    (let ((k (->string k)))
      (and (pair? lst)
           (let recur ((kv #f) (lst lst))
             (if (or kv (null? lst))
                 (if (not kv) default
                     (match kv ((k v) v) (else (cdr kv))))
                 (let ((kv (car lst)))
                   (recur (and (string=? (->string (car kv)) k) kv)
                          (cdr lst)) )))))))


(define fresh (compose string->symbol symbol->string gensym))


(define (find-locals defs)
  (concatenate
   (map (lambda (def) 
          (match def 
                 (('let bnds body) 
                  (let ((bexprs (map second bnds)))
                    (concatenate (list (map first bnds) 
                                       (find-locals bexprs )
                                       (find-locals (list body))))))
                 (('if c t e)      (append (find-locals (list t)) (find-locals (list e))))
                 ((s . rest)       (find-locals rest))
                 (else (list)))) 
        defs)))


(define (enum-bnds expr ax)
  (match expr 
	 (('if . es)        (fold enum-bnds ax es))
	 (('let bnds body)  (enum-bnds body (append (map car bnds) (fold enum-bnds ax (map cadr bnds)))))
	 ((s . es)          (if (symbol? s)  (fold enum-bnds ax es) ax))
	 (else ax)))


(define (enum-freevars expr bnds ax)
  (match expr 
	 (('if . es)  
	  (fold (lambda (x ax) (enum-freevars x bnds ax)) ax es))
	 (('let lbnds body)  
	  (let ((bnds1 (append (map first lbnds) bnds)))
	    (enum-freevars body bnds1 
	     (fold (lambda (x ax) (enum-freevars x bnds ax)) ax 
		   (map second lbnds)))))
	 ((s . es)    (if (symbol? s)  (fold (lambda (x ax) (enum-freevars x bnds ax)) ax es) ax))
	 (id          (if (and (symbol? id) (not (member id bnds)))  (cons id ax) ax))))


(define (sum lst)
  (if (null? lst) lst
      (match lst
	     ((x)   x)
	     ((x y) `(+ ,x ,y))
	     ((x y . rest) `(+ (+ ,x ,y) ,(sum rest)))
	     ((x . rest) `(+ ,x ,(sum rest))))))


(define (if-convert expr)
  (match expr 
	 (('if c t e) 
	  (let ((r (fresh "if")))
	    `(let ((,r (if ,(if-convert c) ,(if-convert t) ,(if-convert e))))  
	       ,r)))
	 (('let bs e)
	  `(let ,(map (lambda (b) `(,(car b) ,(if-convert (cadr b)))) bs) ,(if-convert e)))
	 ((f . es)
	  (cons f (map if-convert es)))
	 ((? atom? ) expr)))

	 
(define (let-enum expr ax)
  (match expr
	 (('let ((x ('if c t e))) y)
	  (let ((ax (fold let-enum ax (list c ))))
	    (if (eq? x y)  (append ax (list (list x `(if ,c ,t ,e)))) ax)))

	 (('let bnds body)  (append ax bnds))

	 (('if c t e)  (let-enum c ax))

	 ((f . es)  (fold let-enum ax es))

	 (else ax)))


(define (let-elim expr)
  (match expr
	 (('let ((x ('if c t e))) y)
	  (if (eq? x y)  y expr))

	 (('let bnds body) body)

	 (('if c t e)  `(if ,(let-elim c) ,(let-lift t) ,(let-lift e)))

	 ((f . es)  `(,f . ,(map let-elim es)))

	 (else expr)))
  

(define (let-lift expr)
  (define (fbnds bnds)
    (let ((bnds0 
	   (fold (lambda (b ax) 
		   (let ((bexpr  (cadr b)))
		     (match bexpr
			    (('let bnds expr) (append bnds ax))
			    (else (append (let-enum bexpr (list)) ax)))))
		 '() bnds)))
      bnds0))
  (let ((expr1
	 (match expr
		(('let bnds expr) 
		 (let ((bnds0 (fbnds bnds))
		       (expr1 
			`(let  ,(map (lambda (b) (list (car b) (let-elim (cadr b)))) bnds)
			   ,(let-lift expr))))
		     (if (null? bnds0) expr1 `(let ,bnds0 ,expr1))))

		(else 
		 (let ((bnds (let-enum expr (list))))
		   (if (null? bnds) 
		       (let-elim expr)
		       (let ((bnds0 (fbnds bnds))
			     (expr1 `(let ,(map (lambda (b) (list (car b) (let-elim (cadr b)))) bnds)
				       ,(let-elim expr))))
			 (if (null? bnds0) expr1 `(let ,bnds0 ,expr1))))))
		)))
    (if (equal? expr expr1) expr1 
	(let-lift expr1))))


(define (add-params-to-fncall expr builtin-fns #!key (name 'params))
  (let recur ((expr expr))
    (match expr 
           (('if c t e)  `(if ,(recur  c) ,(recur  t) ,(recur  e)))
           (('let bs e)
            `(let ,(map (lambda (b) `(,(car b) ,(recur  (cadr b)))) bs) ,(recur  e)))
           ((f . es) 
            (if (member f builtin-fns)
                (cons  f  (map recur  es))
                (let ((es1 (map recur  es)))
                  (cons f (if (equal? (last es1) name) es1 (append es1 `(,name))) ))
                ))
           ((or (? symbol? ) (? atom? )) expr))
    ))


(define (s+ . lst)    (string-concatenate (map ->string lst)))
(define (sw+ lst)     (string-intersperse (filter-map (lambda (x) (and x (->string x))) lst) " "))
(define (slp p lst)   (string-intersperse (map ->string lst) p))
(define nl "\n")
(define (spaces n)    (list->string (list-tabulate n (lambda (x) #\space))))

(define (ppf indent . lst)
  (let ((sp (spaces indent)))
    (for-each (lambda (x)
		(and x (match x 
			      ((i . x1) (if (and (number? i) (positive? i))
					    (for-each (lambda (x) (ppf (+ indent i) x)) x1)
					    (print sp (sw+ x))))
			      (else   (print sp (if (list? x) (sw+ x) x))))))
	      lst)))


(define (transitions-graph n open transitions conserve state-name)
  (let* ((subst-convert  (subst-driver (lambda (x) (and (symbol? x) x)) 
				       nemo:binding? identity nemo:bind nemo:subst-term))
	 (g          (make-digraph n (string-append (->string n) " transitions graph")))
	 (add-node!  (g 'add-node!))
	 (add-edge!  (g 'add-edge!))
	 (out-edges  (g 'out-edges))
	 (in-edges   (g 'in-edges))
	 (node-info  (g 'node-info))
	 (node-list  (let loop ((lst (list)) (tlst transitions))
		       (if (null? tlst)  (delete-duplicates lst eq?)
			   (match (car tlst) 
				  (('-> (and (? symbol?) s0) (and (? symbol?) s1) rate-expr)
				   (loop (cons* s0 s1 lst) (cdr tlst)))
				  (((and (? symbol?) s0) '-> (and (? symbol? s1)) rate-expr)
				   (loop (cons* s0 s1 lst) (cdr tlst)))
				  (('<-> (and (? symbol?) s0) (and (? symbol?) s1) rate-expr1 rate-expr2)
				   (loop (cons* s0 s1 lst) (cdr tlst)))
				  (((and (? symbol?) s0) 'M-> (and (? symbol? s1)) rate-expr1 rate-expr2)
				   (loop (cons* s0 s1 lst) (cdr tlst)))
				  (else
				   (nemo:error 'state-eqs ": invalid transition equation " 
						  (car tlst) " in state complex " n))
				  (else (loop lst (cdr tlst)))))))
	 (node-ids      (list-tabulate (length node-list) identity))
	 (name->id-map  (zip node-list node-ids))
	 (node-subs     (fold (lambda (s ax) (subst-extend s (state-name n s) ax)) subst-empty node-list)))
    ;; insert state nodes in the dependency graph
    (for-each (lambda (i n) (add-node! i n)) node-ids node-list)
    (let* ((nodes  ((g 'nodes)))
	   (conserve (and (pair? conserve) (car conserve)))
	   ;; if a conservation equation is present, we eliminate one
	   ;; transition equation from the system
	   (cvars  (and conserve
                        (enum-freevars (third conserve) '() '())))
	   (cnode  (and cvars
			(find (lambda (s) 
				(let ((n (second s)))
				  (and (member n cvars) 
                                       (if (pair? open) 
                                           (not (member n open)))
                                           (not (eq? n open)))))
			      nodes)))
	   (cname  (and cnode (second cnode)))
	   (cnexpr (and cnode
			(let* ((cvars1   (filter-map (lambda (n) (and (not (eq? n cname)) n)) cvars))
			       (sumvar   (fresh "sum")))
			  `(let ((,sumvar ,(sum cvars1))) (- ,(exact->inexact (first conserve)) ,sumvar)))))
	   (add-tredge (lambda (s0 s1 rexpr1 rexpr2)
			 (let* ((i   (car (alist-ref s0 name->id-map)))
				(j   (car (alist-ref s1 name->id-map)))
				(x0  (if (and cnode (eq? s0 cname)) cnexpr s0))
				(x1  (if (and cnode (eq? s1 cname)) cnexpr s1))
				(ij-expr  `(* ,(subst-convert x0 node-subs)
					      ,(subst-convert rexpr1 node-subs)))
				(ji-expr  (and rexpr2
					       `(* ,(subst-convert x1 node-subs)
						   ,(subst-convert rexpr2 node-subs)))))
			   (add-edge! (list i j ij-expr))
			   (if rexpr2 (add-edge! (list j i ji-expr)))))))
      
      ;; create rate edges in the graph
      (for-each (lambda (e) 
		  (match e
			 (('-> s0 s1 rexpr)  (add-tredge s0 s1 rexpr #f))
			 ((s0 '-> s1 rexpr)  (add-tredge s0 s1 rexpr #f))
			 (('<-> s0 s1 rexpr1 rexpr2)  (add-tredge s0 s1 rexpr1 rexpr2))
			 ((s0 '<-> s1 rexpr1 rexpr2)  (add-tredge s0 s1 rexpr1 rexpr2))
			 ))
		transitions)

      (list g cnode node-subs))))


(define (state-conseqs n transitions conseqs state-name)
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
				      (((and (? symbol?) s0) 'M-> (and (? symbol? s1)) rate-expr1 rate-expr2)
				       (loop (cons* s0 s1 lst) (cdr tlst)))
				      (else
				       (nemo:error 'nemo:state-conseq ": invalid transition equation " 
						   (car tlst) " in state complex " n))
				      (else (loop lst (cdr tlst)))))))
	 (state-subs     (fold (lambda (s ax) (subst-extend s (state-name n s) ax)) subst-empty state-list))
	 (conseqs1        (map (lambda (conseq) (match conseq ((i '= . expr) `(,i = . ,(subst-convert expr state-subs)))))
			      conseqs)))
    (list n conseqs1)))


(define (make-output-fname dirname sysname suffix . rest) 
  (let-optionals rest ((x #t))
    (and x
	 (let ((dirname (if (string? x) x dirname)))
	   (let ((fname (s+ sysname suffix)))
	     (or (and dirname (make-pathname dirname fname)) fname)))
	 )))



(define (alist-update k v lst #!optional (cmp eqv?))
  (let loop ((lst lst))
    (if (null? lst)
        (list (cons k v))
        (let ((a (##sys#slot lst 0)))
          (cond ((not (pair? a))
                 (error 'alist-update "bad argument type" a))
                ((cmp (##sys#slot a 0) k)
                 (cons (cons k v) (##sys#slot lst 1)))
                (else
                 (cons (cons (##sys#slot a 0) (##sys#slot a 1))
                       (loop (##sys#slot lst 1)))))))))




(define (bucket-partition p lst)
  (let loop ((lst lst) (ax (list)))
    (if (null? lst) ax
	(let ((x (car lst)))
	  (let bkt-loop ((old-bkts ax) (new-bkts (list)))
	    (if (null? old-bkts) (loop (cdr lst) (cons (list x) new-bkts))
		(if (p x (caar old-bkts ))
		    (loop (cdr lst) (append (cdr old-bkts) (cons (cons x (car old-bkts)) new-bkts)))
		    (bkt-loop (cdr old-bkts) (cons (car old-bkts) new-bkts)))))))))


(define (expeuler dt name rhs)
  (define (isname? x) (equal? x name))
  (let ((res 
	 (match rhs
		((or ('- A ('* B (and x (? isname?))))
		     ('+ ('neg ('* B (and x (? isname?)))) A))
		 (let ((xexp (string->symbol (s+ x 'exp))))
		   `(let ((,xexp (exp (* (neg ,B) ,dt))))
		      (+ (* ,x ,xexp)  (* (- 1 ,xexp) (/ ,A ,B))))))

		((or ('- A ('* (and x (? isname?)) . B))
		     ('+ ('neg ('* (and x (? isname?)) . B)) A))
		 (let ((xexp (string->symbol (s+ x 'exp)))
		       (B1   (if (null? (cdr B)) (car B) `(* ,@B))))
		   `(let ((,xexp (exp (* (neg ,B1) ,dt))))
		      (+ (* ,x ,xexp)  (* (- 1 ,xexp) (/ ,A ,B1))))))
		
		(('+ ('neg ('* (and x1 (? isname?)) Alpha))
		     ('* ('- 1 (and x2 (? isname?))) Beta))
		 (let ((A  Alpha)
		       (B  `(+ ,Alpha ,Beta)))
		   (let ((xexp (string->symbol (s+ x1 'exp))))
		     `(let ((,xexp (exp (* (neg ,B) ,dt))))
			(+ (* ,x1 ,xexp) (* (- 1 ,xexp) (/ ,A ,B)))))))
		
		(('let bnds body)
		 `(let ,bnds ,(expeuler dt name body)))
		
		(else (nemo:error 'nemo:expeuler ": unable to rewrite equation " rhs 
				  "in exponential Euler form")))))

    res))



(define (reaction-eq n open transitions conserve make-name make-state-name rhsexpr canonicalize-expr )
  (if (symbol? open)
      (list (make-name n) (let ((o (make-state-name n open))) o))
      (list (make-name n) (sum (map (lambda (x) (make-state-name n x)) open)))
      ))


(define (state-power sys n)
  (let ((en (hash-table-ref sys n)))
    (if (nemo:quantity? en)
	(cases nemo:quantity en
	       (REACTION   (name initial open transitions conserve power u)  power)
	       (RATE       (name initial rhs power u) power)
               (TRANSIENT  (name initial rhs asgn power u) power)
	       (else  #f))  
        #f)))

(define (quantity-unit sys n)
  (let ((en (hash-table-ref sys n)))
    (if (nemo:quantity? en)
	(cases nemo:quantity en
	       (REACTION   (name initial open transitions conserve power u)  u)
	       (RATE       (name initial rhs power u) u)
               (TRANSIENT  (name initial rhs asgn power u) u)
               (CONST      (name value u ) u)
               (EXTERNAL   (ln n ns u) u)
	       (else  #f))  
        #f)))


(define (sys->external-eq-defs sys  make-name rhsexpr canonicalize-expr #!key (namespace-filter #f))
  (fold  (lambda (n ax) 
	   (let ((en (hash-table-ref sys n)))
	     (if (nemo:quantity? en)
		 (cases nemo:quantity en
			(EXTERNAL (local-name name namespace u)  
                                  (if (and (not (equal? local-name name)) 
                                           (or (not namespace-filter)
                                               (and (symbol? namespace) (procedure? namespace-filter) 
                                                    (namespace-filter namespace))))
                                      (cons (asgn-eq local-name name make-name rhsexpr canonicalize-expr) ax) ax))
			(else  ax))
		 ax)))
	 (list) (hash-table-keys sys)))


(define (reaction-transition-eq-defs n initial open transitions conserve power method 
                                     make-name make-state-name rhsexpr canonicalize-expr )
    (match-let (((g cnode node-subs)
                 (transitions-graph n open transitions conserve make-state-name)))
     (let* ((out-edges  (g 'out-edges))
	    (in-edges   (g 'in-edges))
	    (nodes      ((g 'nodes))))
       ;; generate differential equations for each state in the transitions system
      (let ((eqs    (fold (lambda (s ax) 
			    (if (and cnode (= (first cnode) (first s) )) ax
				(let* ((out   (out-edges (first s)))
				       (in    (in-edges (first s)))
				       (open? (eq? (second s) open))
				       (name  (make-name (lookup-def (second s) node-subs))))
				  (let* ((rhs1  (cond ((and (not (null? out)) (not (null? in)))
						       `(- ,(sum (map third in))
                                                           ,(sum (map third out))))
						      ((and (not (null? out)) (null? in))
						       `(neg ,(sum (map third out))))
						      ((and (null? out) (not (null? in)))
						       (sum (map third in)))))
					 (fbody0 (rhsexpr rhs1))
					 (fbody1 (case method
						   ((expeuler) (canonicalize-expr (expeuler 'dt name fbody0)))
						   (else       (canonicalize-expr fbody0)))))
				    (cons (list name fbody1) ax))
				  )))
			  (list) nodes)))
	eqs))
     ))



(define (asgn-eq n rhs make-name rhsexpr canonicalize-expr )
  (let* ((fbody   (rhsexpr rhs))
	 (fbody1  (canonicalize-expr fbody)))
    (list (make-name n) fbody1)))


(define (poset->asgn-eq-defs poset sys make-name rhsexpr canonicalize-expr)
  (fold-right
   (lambda (lst ax)
     (fold  (lambda (x ax) 
	      (match-let (((i . n)  x))
			 (let ((en (hash-table-ref sys n)))
			   (if (nemo:quantity? en)
			       (cases nemo:quantity en
				      (ASGN  (name value rhs) (cons (asgn-eq name rhs make-name rhsexpr canonicalize-expr) ax))
				      (else  ax))
			       ax))))
	    ax lst))
   (list) poset))


(define (poset->rate-eq-defs poset sys method make-name make-state-name rhsexpr canonicalize-expr 
                             #!key (kinetic '()))
  (fold-right
   (lambda (lst ax)
     (fold  (lambda (x ax) 
	      (match-let (((i . n)  x))
			 (let ((en (hash-table-ref sys n)))
			   (if (and (not (member n kinetic)) (nemo:quantity? en))
			       (cases nemo:quantity en

				      (REACTION  (name initial open transitions conserve power u) 
						 (append (reaction-transition-eq-defs name initial open transitions 
                                                                                      conserve power method
                                                                                      make-name make-state-name
                                                                                      rhsexpr canonicalize-expr)
                                                         ax))
				      
				      (RATE (name initial rhs power u)
					    (let ((fbody0  (rhsexpr rhs))
						  (dy      (make-name name) ))
					      (case method
						((expeuler)  
						 (cons (list dy (canonicalize-expr (expeuler 'dt name fbody0))) 
						       ax))
						(else
						 (cons (list dy (canonicalize-expr fbody0)) ax)))))

				      (TRANSIENT  (name initial rhs asgn power u) 
					    (let ((fbody0  (rhsexpr rhs))
						  (dy      (make-name name) ))
					      (case method
						((expeuler)  
						 (cons (list dy (canonicalize-expr (expeuler 'dt name fbody0))) 
						       ax))
						(else
						 (cons (list dy (canonicalize-expr fbody0)) ax)))))

				      (else  ax))
			       ax))))
	    ax lst))
   (list) poset))


(define (poset->reaction-eq-defs poset sys make-name make-state-name rhsexpr canonicalize-expr)
  (fold-right
   (lambda (lst ax)
     (fold  (lambda (x ax) 
	      (match-let (((i . n)  x))
			 (let ((en (hash-table-ref sys n)))
			   (if (nemo:quantity? en)
			       (cases nemo:quantity en
				      (REACTION  (name initial open transitions conserve power u) 
						 (cons (reaction-eq name open transitions conserve 
                                                                    make-name make-state-name rhsexpr canonicalize-expr ) ax))
				      (else  ax))
			       ax))))
	    ax lst))
   (list) poset))


(define (poset->transient-event-defs poset sys method make-name make-state-name rhsexpr canonicalize-expr builtin-fns)

  (fold-right
   (lambda (lst ax)
     (fold  (lambda (x ax) 
	      (match-let (((i . n)  x))
			 (let ((en (hash-table-ref sys n)))
			   (if (nemo:quantity? en)
			       (cases nemo:quantity en
				      (TRANSIENT  (name initial rhs asgn power u) 
					    (let ((asgnbody0     (rhsexpr asgn))
						  (a             (make-name name) ))
                                              (cons (list a (canonicalize-expr asgnbody0))
                                                    ax)))
                           
				      (else  ax))
			       ax))))
	    ax lst))
   (list) poset))



(define (state-init n init make-name rhsexpr canonicalize-expr)
  (let* ((init  (rhsexpr init))
	 (init1 (canonicalize-expr init)))
    (list (make-name n) init1)))



(define (poset->init-defs poset sys make-name make-state-name rhsexpr canonicalize-expr)
  (fold-right
   (lambda (lst ax)
     (fold-right
      (lambda (x ax) 
	      (match-let (((i . n)  x))
			 (let ((en (hash-table-ref sys n)))
			   (if (nemo:quantity? en)
			       (cases nemo:quantity en

				      (REACTION  (name initial open transitions conserve power u)
                                                 (let ((o (if (pair? open) (car open) open)))
                                                   (if (nemo:rhs? initial)
                                                       (cons* (state-init name initial make-name rhsexpr canonicalize-expr) 
                                                              (state-init (make-state-name name o) name make-name rhsexpr canonicalize-expr) ax) 
                                                       ax)))

				      (RATE  (name initial rhs power u)
					     (if (and initial (nemo:rhs? initial))
						 (cons (state-init name initial make-name rhsexpr canonicalize-expr) ax) 
						 ax))

                                      (TRANSIENT  (name initial rhs asgn power u) 
                                                  (if (and initial (nemo:rhs? initial))
                                                      (cons (state-init name initial make-name rhsexpr canonicalize-expr) ax) 
                                                      ax))

				      (else  ax))
			       ax))))
	    ax lst))
   (list) poset))


(define (poset->state-conserve-eq-defs poset sys make-name make-state-name)
  (fold-right
   (lambda (lst ax)
     (fold  (lambda (x ax) 
	      (match-let (((i . n)  x))
			 (let ((en (hash-table-ref sys n)))
			   (if (nemo:quantity? en)
			       (cases nemo:quantity en
				      (REACTION (name initial open transitions conserve power u)
						(if (and (list? conserve) (every nemo:conseq? conserve))
						    (cons (state-conseqs (make-name name) transitions conserve
									make-state-name) ax) 
						    ax))
				      (else  ax))
			       ax))))
	    ax lst))
   (list) poset))


;; * variants of the procedures above rewrite model functions to explicitly pass model parameters

(define (state-init* n init make-name rhsexpr canonicalize-expr builtin-fns)
  (let* ((init  (rhsexpr init))
	 (init1 (canonicalize-expr
		 (add-params-to-fncall init builtin-fns))))
    (list (make-name n) init1)))


(define (poset->init-defs* poset sys make-name make-state-name rhsexpr canonicalize-expr builtin-fns)
  (fold-right
   (lambda (lst ax)
     (fold-right
      (lambda (x ax) 
	      (match-let (((i . n)  x))
			 (let ((en (hash-table-ref sys n)))
			   (if (nemo:quantity? en)
			       (cases nemo:quantity en
				      (REACTION  (name initial open transitions conserve power u)
						 (if (nemo:rhs? initial)
						     (cons* (state-init* name initial make-name rhsexpr canonicalize-expr builtin-fns) 
							    (state-init* (make-state-name name open) name make-name rhsexpr canonicalize-expr builtin-fns) ax) 
						     ax))

				      (RATE  (name initial rhs power u)
					     (if (and initial (nemo:rhs? initial))
						 (cons (state-init* name initial make-name rhsexpr canonicalize-expr builtin-fns) ax) 
						 ax))

                                      (TRANSIENT  (name initial rhs asgn power u) 
                                                  (if (and initial (nemo:rhs? initial))
                                                      (cons (state-init* name initial make-name rhsexpr canonicalize-expr builtin-fns) ax)
                                                      ax))

				      (else  ax))
			       ax))))
	    ax lst))
   (list) poset))


(define (asgn-eq* n rhs make-name rhsexpr canonicalize-expr builtin-fns)
  (let* ((fbody   (rhsexpr rhs))
	 (fbody1  (canonicalize-expr 
		   (add-params-to-fncall fbody builtin-fns))))
    (list (make-name n) fbody1)))


(define (poset->asgn-eq-defs* poset sys make-name rhsexpr canonicalize-expr builtin-fns)
  (fold-right
   (lambda (lst ax)
     (fold  (lambda (x ax) 
	      (match-let (((i . n)  x))
			 (let ((en (hash-table-ref sys n)))
			   (if (nemo:quantity? en)
			       (cases nemo:quantity en
				      (ASGN  (name value rhs) (cons (asgn-eq* name rhs make-name rhsexpr canonicalize-expr builtin-fns) ax))
				      (else  ax))
			       ax))))
	    ax lst))
   (list) poset))


(define (reaction-transition-eq-defs* n initial open transitions conserve power method 
                                      make-name make-state-name rhsexpr canonicalize-expr
                                      builtin-fns)
    (match-let (((g cnode node-subs)
                 (transitions-graph n open transitions conserve make-state-name)))
     (let* ((out-edges  (g 'out-edges))
	    (in-edges   (g 'in-edges))
	    (nodes      ((g 'nodes))))
       ;; generate differential equations for each state in the transitions system
      (let ((eqs    (fold (lambda (s ax) 
			    (if (and cnode (= (first cnode) (first s) )) ax
				(let* ((out   (out-edges (first s)))
				       (in    (in-edges (first s)))
				       (open? (eq? (second s) open))
				       (name  (make-name (lookup-def (second s) node-subs))))
				  (let* ((rhs1  (cond ((and (not (null? out)) (not (null? in)))
						       `(+ (neg ,(sum (map third out)))
							   ,(sum (map third in))))
						      ((and (not (null? out)) (null? in))
						       `(neg ,(sum (map third out))))
						      ((and (null? out) (not (null? in)))
						       (sum (map third in)))))
					 (fbody0 (rhsexpr rhs1))
					 (fbody1 (case method
						   ((expeuler) (canonicalize-expr (expeuler 'dt name fbody0)))
						   (else       (canonicalize-expr fbody0)))))
				    (cons (list name (add-params-to-fncall fbody1 builtin-fns)) ax))
				  )))
			  (list) nodes)))
	eqs))
     ))


(define (poset->rate-eq-defs* poset sys method make-name make-state-name rhsexpr canonicalize-expr builtin-fns
                              #!key (kinetic '()))

  (fold-right
   (lambda (lst ax)
     (fold  (lambda (x ax) 
	      (match-let (((i . n)  x))
			 (let ((en (hash-table-ref sys n)))
			   (if (and (not (member n kinetic)) (nemo:quantity? en))
			       (cases nemo:quantity en

				      (REACTION  (name initial open transitions conserve power u)  
						 (append (reaction-transition-eq-defs* name initial open transitions 
                                                                                       conserve power method
                                                                                       make-name make-state-name
                                                                                       rhsexpr canonicalize-expr
                                                                                       builtin-fns) ax))
				      
				      (RATE (name initial rhs power u)
					    (let ((fbody0  (rhsexpr rhs))
						  (dy      (make-name name) ))
					      (case method
						((expeuler)  
						 (cons (list dy (add-params-to-fncall
                                                                 (canonicalize-expr (expeuler 'dt name fbody0))
                                                                 builtin-fns))
						       ax))
						(else
						 (cons (list dy (add-params-to-fncall
                                                                 (canonicalize-expr fbody0)
                                                                 builtin-fns)) ax)))))


				      (TRANSIENT  (name initial rhs asgn power u) 
					    (let ((fbody0  (rhsexpr rhs))
						  (dy      (make-name name) ))
					      (case method
						((expeuler)  
						 (cons (list dy (add-params-to-fncall
                                                                 (canonicalize-expr (expeuler 'dt name fbody0)) 
                                                                 builtin-fns))
						       ax))
						(else
						 (cons (list dy (add-params-to-fncall 
                                                                 (canonicalize-expr fbody0) 
                                                                 builtin-fns))
                                                       ax)))))


				      (else  ax))
			       ax))))
	    ax lst))
   (list) poset))

(define (poset->reaction-eq-defs* poset sys make-name make-state-name rhsexpr canonicalize-expr)
  (fold-right
   (lambda (lst ax)
     (fold  (lambda (x ax) 
	      (match-let (((i . n)  x))
			 (let ((en (hash-table-ref sys n)))
			   (if (nemo:quantity? en)
			       (cases nemo:quantity en
				      (REACTION  (name initial open transitions conserve power u) 
						 (cons (reaction-eq name open transitions conserve make-name make-state-name rhsexpr canonicalize-expr ) ax))
				      (else  ax))
			       ax))))
	    ax lst))
   (list) poset))


(define (poset->transient-event-defs* poset sys method make-name make-state-name rhsexpr canonicalize-expr builtin-fns)

  (fold-right
   (lambda (lst ax)
     (fold  (lambda (x ax) 
	      (match-let (((i . n)  x))
			 (let ((en (hash-table-ref sys n)))
			   (if (nemo:quantity? en)
			       (cases nemo:quantity en
				      (TRANSIENT  (name initial rhs asgn power u) 
					    (let ((asgnbody0     (rhsexpr asgn))
						  (a             (make-name name) ))
                                              (cons (list a 
                                                          (add-params-to-fncall
                                                           (canonicalize-expr asgnbody0)
                                                           builtin-fns))
                                                    ax)))
                           
				      (else  ax))
			       ax))))
	    ax lst))
   (list) poset))




(define LOG10E 0.434294481903252)
(define LOG2E  1.44269504088896)


(define template-dir
  (let* ((shared-dir (chicken-home))
         (nemo-dir (make-pathname shared-dir "nemo")))
    (make-pathname nemo-dir "templates")))


(define (tenv-enter x env)
  (let ((k (car x)) (v (cdr x)))
    (cons (cons k (if (null? v) 
                      (ersatz:Tlist '()) 
                      (ersatz:sexpr->tvalue v))) 
          env)))

(define (instantiate-template tmpl tmpl-vars)
    (let ((ctx (ersatz:init-context models: tmpl-vars )))
      (display
       (ersatz:eval-statements tmpl
                               env: (ersatz:template-std-env 
                                     search-path: `(,template-dir)
                                     autoescape: #f)
                               models: tmpl-vars ctx: ctx))
      ))

(define (instantiate-template* user-templates template-name template-vars)
  (let ((tmpl (assoc (->string template-name) user-templates string=?)))
    (if (not tmpl)
	(error 'nemo "template not found" template-name))
    (let ((ctx (ersatz:init-context models: template-vars )))
      (display
       (ersatz:eval-statements (caddr tmpl)
			       env: (ersatz:template-std-env)
			       models: template-vars ctx: ctx))
      )))

(define template-lexer (ersatz:make-lexer* 'nemo-ersatz-lexer))

(define (read-template str)
  (let ((env (ersatz:template-std-env lexer: template-lexer)))
    (ersatz:statements-from-string env str)))


)
