;;
;; NEMO: a description language for models of neuronal ionic currents.
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

(import files setup-api srfi-1 srfi-4 srfi-13 srfi-69)


(require-extension nemo-core nemo-macros nemo-hh nemo-vclamp nemo-iclamp 
                   nemo-utils nemo-units nemo-fetch)
(require-library iexpr ersatz-lib)
(require-extension datatype matchable lalr-driver
		   ssax sxml-transforms sxpath sxpath-lolevel 
		   getopt-long)
(import (prefix iexpr iexpr: )
	(prefix ersatz-lib ersatz: )
	)

(define user-template-list?
  (lambda (ts) 
    (every (lambda (x) (and (string? (car x))
			    (every string? (cadr x))
			    (every ersatz:tstmt? (caddr x)))) ts)))


(define-datatype nemo:model nemo:model?
  (ModelSource (source-path string?) (in-format symbol?) (name symbol?) 
	       (decls list?) 
	       (user-templates user-template-list?)
	       (iexpr boolean?) (parse-expr procedure?))
  (SingleModel (source-path string?) (in-format symbol?) (name symbol?) 
	       (sys hash-table?) (decls list?) (user-templates user-template-list?)
	       (iexpr boolean?) (parse-expr procedure?))
  (ModelPart   (source-path string?) (in-format symbol?) (name symbol?) (part-name symbol?) 
	       (sys hash-table?) (decls list?) (parent-decls list?)
               (user-templates user-template-list?)
	       (iexpr boolean?) (parse-expr procedure?))
  )
 

(define nemo-nmodl?       (extension-information 'nemo-nmodl))  
(define nemo-matlab?      (extension-information 'nemo-matlab)) 
(define nemo-nest?        (extension-information 'nemo-nest)) 
(define nemo-pyparams?    (extension-information 'nemo-pyparams)) 

(if nemo-nmodl?   (use nemo-nmodl))
(if nemo-matlab?  (use nemo-matlab))
(if nemo-nest?    (use nemo-nest))
(if nemo-pyparams?    (use nemo-pyparams))

(define (lookup-def k lst . rest)
  (let-optionals rest ((default #f))
      (let ((kv (assoc k lst)))
	(if (not kv) default
	    (match kv ((k v) v) (else (cdr kv)))))))

(define ($ x)  (and x (string->symbol (->string x))))

;;; Procedures for string concatenation and pretty-printing

(define (s+ . lst)    (string-concatenate (map ->string lst)))
(define (sw+ lst)     (string-intersperse (filter-map (lambda (x) (and x (->string x))) lst) " "))
(define (s\ p . lst)  (string-intersperse (map ->string lst) p))
(define (slp p lst)   (string-intersperse (map ->string lst) p))
(define nl "\n")


(define (warn port message . specialising-msgs)
  (print-error-message message (current-output-port) "Warning")
  (print (string-concatenate (map ->string specialising-msgs))))

;;; Error procedure for the XML parser

(define (parser-error port message . specialising-msgs)
  (error (string-append message (string-concatenate (map ->string specialising-msgs)))))

(define ssax:warn warn)

(define opt-defaults
  `(
    (nmodl-kinetic . all)
    (nmodl-method . cnexp)
    ))

(define (defopt x)
  (lookup-def x opt-defaults))

(define opt-grammar
  `(
    (input-format
     "specify input format (nemo, xml, ixml, sxml, s-exp)"
     (single-char #\i)
     (value (required FORMAT)
	    (transformer ,string->symbol)))

    (partition
     "partition model source into individual parts for each current"
     (single-char #\p))

    (surface-xml
     "write surface XML translation of input to file (default: <model-name>.xml)"
     (value (optional DIRNAME)
	    ))

    (surface-nineml
     "write NineML-compatible translation of input to file (default: <model-name>.xml)"
     (value (optional DIRNAME)
	    ))

    (plain
     "write plain text output to file (default: <model-name>.txt)"
     (value (optional DIRNAME)
	    ))

    (xml
     "write XML output to file (default: <model-name>.xml)"
     (value (optional DIRNAME)
	    ))

    (sxml
     "write SXML output to file (default: <model-name>.sxml)"
     (value (optional DIRNAME)
	    ))

    (hh-markov
     "convert HH rate equations to Markov chain form")

    (print-default-units
     "print default units used for target platform")

    (default-units
     "set default units used for target platform"
     (value (required QUANTITY:UNIT)
            (transformer 
             ,(lambda (x) 
                (map (lambda (x) 
                       (match-let (((dimstr unitstr) (string-split x ":")))
                                  (let ((dimsym (string->symbol dimstr))
                                        (unitsym (string->symbol unitstr)))
                                    (let* ((alldims (map (lambda (x) 
                                                           (cons (nemo:quantity-name (car x)) (car x)))
                                                         (nemo:default-units)))
                                           (dim (lookup-def dimsym alldims))
                                           (u   (lookup-def unitsym nemo:basic-units)))
                                      (if (not (and u (= (nemo:quantity-int (nemo:unit-dims u)))
                                                    (nemo:quantity-int dim)))
                                          (error 'default-units "invalid unit for given quantity"
                                                 unitsym dimsym)
                                          (cons dim u))))
                                  ))
                          (string-split x ","))))
             )
            )

    ,@(if nemo-nest? 
	  `(
	    (nest
	     "write NEST output files <model-name>.cpp and <model-name>.h in the given directory (default: .)" 
	     (value (optional DIRNAME)))

	    (nest-method
	     "specify NEST integration method (gsl, cvode, ida)"
	     (value (required METHOD)
		    (transformer ,string->symbol)))

	    (nest-ss-method
	     "specify NEST steady state solving method (gsl, kinsol)"
	     (value (required METHOD)
		    (transformer ,string->symbol)))

	    (nest-abstol
	     "specify NEST absolute tolerance (default is 1e-7)"
	     (value (required NUMBER)
		    (transformer ,string->number)))

	    (nest-reltol
	     "specify NEST relative tolerance (default is 1e-7)"
	     (value (required NUMBER)
		    (transformer ,string->number)))

	    (nest-maxstep
	     "specify NEST maximum step size (default is provided by the NEST interpreter)"
	     (value (required NUMBER)
		    (transformer ,string->number)))
	    )
	  `())

    ,@(if nemo-pyparams? 
	  `(
	    (pyparams
	     "write Python representation of parameters to given file (default: <model-name>.py)"
	     (value (optional DIRNAME)))
	    )
	  `())

    ,@(if nemo-matlab? 
	  `((matlab
	     "write MATLAB output in the given directory (default: .)"
	     (value (optional DIRNAME)))

	    (octave
	     "write Octave output to given file (default: <model-name>.m)"
	     (value (optional DIRNAME)))
		     
	    (octave-method
	     "specify Octave integration method (lsode, odepkg, or cvode)"
	     (value (required METHOD)
		    (transformer ,string->symbol)))
	    )
	  `())

    ,@(if nemo-nmodl?
	  `(
	     (nmodl      "write NMODL output to file (default: <model-name>.mod)"
			 (value (optional DIRNAME)))

	     (nmodl-kinetic  ,(s+ "use NMODL kinetic equations for the given reactions "
				  "(or for all reactions)")
			     (value (optional STATES)
				    (default  ,(defopt 'nmodl-kinetic))
				    (transformer 
				     ,(lambda (x) 
					(if (string=? x "all") 'all
					    (map string->symbol (string-split x ",")))))))
	     
	     (nmodl-method   "specify NMODL integration method"
			     (value (required METHOD)
				    (transformer ,string->symbol)))
	     )
	    `())

    (vclamp-hoc
     "write voltage clamp scripts to HOC file (default: <model-name>.hoc)"
     (value (optional DIRNAME)
	    ))

    (vclamp-octave
     "write voltage clamp script to Octave file (default: <model-name>_vclamp.m)"
     (value (optional DIRNAME)
	    ))

    (iclamp-hoc
     "write current pulse injection scripts to HOC file (default: <model-name>.hoc)"
     (value (optional DIRNAME)
	    ))

    (iclamp-nest
     "write current pulse injection script to NEST SLI file (default: <model-name>.sli)"
     (value (optional DIRNAME)
	    ))

    (template
     "instantiate the given template from the model file by setting the given variables to the respective values"
     (value (required "NAME[:VAR=VAL...]"))
     (multiple #t)
     )

    (template-prefix 
     "output instantiated templates to <PREFIX><template_name> (default is <model-name>_<template_name>)"
     (value (required PREFIX)
	    ))

    (dump-template-env "print the template environment used for code generation (only nmodl or nest backend)")

    (debug "print additional debugging information")

    (version "print the current version and exit")

    (help         (single-char #\h))


    ))


;; Use args:usage to generate a formatted list of options (from OPTS),
;; suitable for embedding into help text.
(define (nemo:usage)
  (print "Usage: " (car (argv)) "  <list of files to be processed> [options...] ")
  (newline)
  (print "The following options are recognized: ")
  (newline)
  (print (parameterize ((indent 5) (width 30)) (usage opt-grammar)))
  (exit 1))


;; Process arguments and collate options and arguments into OPTIONS
;; alist, and operands (filenames) into OPERANDS.  You can handle
;; options as they are processed, or afterwards.

(define opts    (getopt-long (command-line-arguments) opt-grammar))
(define opt     (make-option-dispatch opts opt-grammar))


(define (nmlb:sxpath query doc)
  ((sxpath query '((nmlb . "http://www.nineml.org/Biophysics"))) doc))

(define (ncml:sxpath query doc)
  ((sxpath query '((ncml . "ncml"))) doc))

(define (ncml:car-sxpath query doc)
  (let ((lst ((sxpath query '((ncml . "ncml") )) doc)))
    (car lst)))

(define (ncml:if-car-sxpath query doc)
  (let ((lst ((sxpath query '((ncml . "ncml") )) doc)))
    (and (not (null? lst)) (car lst))))

(define (ncml:if-sxpath query doc)
  (let ((lst ((sxpath query '((ncml . "ncml") )) doc)))
    (and (not (null? lst)) lst)))

(define (ncml-binding->binding node)
  (match node
    (('ncml:bnd ('@ ('id id)) ('ncml:expr expr))
     `(,($ id) ,(ncml-expr->expr expr)))
    (else (error 'ncml-binding->binding "invalid binding " node))))
  
(define (ncml-expr->expr node)
  (match node
	 ((? number?)    node)
	 ((? string?)    (sxml:number node))
	 (('ncml:id id)  ($ id))
	 (('ncml:apply ('@ ('id id)) . args)  (cons ($ id) (map ncml-expr->expr args)))
	 (('ncml:let ('ncml:bnds . bnds) ('ncml:expr body))
	  `(let ,(map ncml-binding->binding bnds) ,(ncml-expr->expr body)))
	 (((and op (? symbol?)) . args)
	  (cons (ncml-op->op op) (map ncml-expr->expr args)))
	 (else (error 'ncml-expr->expr "unknown expression " node))))
  

(define (ncml-op->op op)
  (case op
    ((ncml:sum)    '+)
    ((ncml:sub)    '-)
    ((ncml:mul)    '*)
    ((ncml:div)    '/)
    ((ncml:gt)     '>)
    ((ncml:lt)     '<)
    ((ncml:lte)    '<=)
    ((ncml:gte)    '>=)
    ((ncml:eq)     '=)
    (else          (match (string-split (->string op) ":")
			  ((pre op)  ($ op))
			  (else (error 'ncml-op->op "invalid operator" op))))))


(define (nemo-constructor name declarations parse-expr)
  (let* ((nemo   (make-nemo-core))
	 (sys    ((nemo 'system) name))
	 (qs     (eval-nemo-system-decls nemo name sys declarations parse-expr: parse-expr)))
    (list sys nemo qs)))


(define (sexp->model-decls doc)
  (match doc
	 ((or ('nemo-model model-name model-decls)
	      ('nemo-model (model-name . model-decls)))
	  (list model-name model-decls))
	 ((or ('nemo-model model-name model-decls user-templates)
	      ('nemo-model (model-name . model-decls) user-templates))
	  (list model-name model-decls 
		(map (lambda (x) (list (->string (car x)) 
				       (map ->string (cadr x))
				       (read-template (caddr x))))
			     user-templates)))
	 (else (error 'sexp->model "unknown model format"))
	 ))


(define (sexp-model-decls->model options model-name model-decls parse-expr)
  (let* ((model+nemo  (nemo-constructor model-name model-decls parse-expr))
	 (model (first model+nemo))
	 (nemo  (second model+nemo)))
    (let ((model-1 (nemo:hh-transformer model (alist-ref 'hh-markov options) parse-expr))) 
      (if (assoc 'depgraph options) (print "dependency graph: " ((nemo 'depgraph*) model-1)))
      (if (assoc 'exports options)  (print "exports: " ((nemo 'exports) model-1)))	
      (if (assoc 'imports options)  (print "imports: " ((nemo 'imports) model-1)))
      (if (assoc 'components options)
	  (for-each (lambda (x) 
		      (print "component " x ": " ((nemo 'component-exports) model-1 (second x)))
		      (print "component " x " subcomponents: " ((nemo 'component-subcomps) model-1 (second x))))
		    ((nemo 'components) model-1)))
      model-1)))
	 

(define model->nmodl 
  (if nemo-nmodl?
      (lambda (options model)
	(nemo:nmodl-translator model
                               (lookup-def 'method options) 
			       (lookup-def 'kinetic options) 
                               (lookup-def 'dump-template-env options) ))
      (lambda (options model) 
	(void))))


(define model->nest 
  (if nemo-nest?
      (lambda (options model)
	(nemo:nest-translator model 
                              (lookup-def 'dirname options) 
                              (lookup-def 'method options) 
                              (lookup-def 'ss-method options) 
                              (lookup-def 'abstol options) 
                              (lookup-def 'reltol options) 
                              (lookup-def 'maxstep options)
                              (lookup-def 'dump-template-env options) ))
      (lambda (options model) 
	(void))))

(define model->pyparams 
  (if nemo-pyparams?
      (lambda (options model)
	(nemo:pyparams-translator (list model) 
                                  (lookup-def 'mode options) 
                                  (lookup-def 'filename options)))
      (lambda (options model) 
	(void))))


(define model->matlab 
  (if nemo-matlab?
      (lambda (options model)
	(nemo:matlab-translator model #f (lookup-def 'dirname options)))
      (lambda (options model) 
	(void))))


(define model->vclamp-hoc 
  (lambda (options model)
    (nemo:vclamp-translator model 'hoc (lookup-def 'filename options))))


(define model->vclamp-octave 
  (lambda (options model)
    (nemo:vclamp-translator model 'matlab 
                            (lookup-def 'filename options)
                            (lookup-def 'octave-method options))))


(define model->iclamp-hoc 
  (lambda (options model)
    (nemo:iclamp-translator model 'hoc (lookup-def 'filename options))))

(define model->iclamp-nest 
  (lambda (options model)
    (nemo:iclamp-translator model 'nest (lookup-def 'filename options))))


(define model->octave 
  (if nemo-matlab?
      (lambda (options model)
	(nemo:octave-translator model 
				(lookup-def 'filename options)
				(lookup-def 'dirname options)))
      (lambda (options model) 
	(void))))


(define (transition->ncml-transition x)
  (match x
	 (('-> src dst rate) 
	  `((ncml:transition (@ (src ,src) (dst ,dst))  (ncml:rate ,(expr->ncml-expr rate)))))
	 ((src '-> dst rate) 
	  `((ncml:transition (@ (src ,src) (dst ,dst))  (ncml:rate ,(expr->ncml-expr rate)))))
	 (('<-> src dst rate1 rate2) 
	  `((ncml:transition (@ (src ,src) (dst ,dst))  (ncml:rate ,(expr->ncml-expr rate1)))
	    (ncml:transition (@ (src ,dst) (dst ,src))  (ncml:rate ,(expr->ncml-expr rate2)))))
	 ((src '<-> dst rate1 rate2) 
	  `((ncml:transition (@ (src ,src) (dst ,dst))  (ncml:rate ,(expr->ncml-expr rate1)))
	    (ncml:transition (@ (src ,dst) (dst ,src))  (ncml:rate ,(expr->ncml-expr rate2)))))
	 (else (error 'transition->ncml-transition "invalid transition " x))))


(define (conseq->ncml-conseq parse-expr)
  (lambda (x)
    (match x 
	   (((and i (? integer?)) '= rhs)
	    `(ncml:conseq (@ (val ,(->string i))) 
			 (ncml:expr ,(expr->ncml-expr (parse-expr rhs)))))
	   (else (error 'conseq->ncml-conseq "invalid linear equation " x)))))


(define builtin-fns
  `(+ - * / pow neg abs atan asin acos sin cos exp ln
      sqrt tan cosh sinh tanh hypot gamma lgamma log10 log2 log1p ldexp cube
      > < <= >= = and or round ceiling floor max min))


(define (binding->ncml-binding bnd)
  (match bnd
	 ((id expr)  `(ncml:bnd (@ (id ,id)) (ncml:expr ,(expr->ncml-expr expr))))
	 (else (error 'binding->ncml-binding "invalid binding " bnd))))

  
(define (expr->ncml-expr x)
  (match x
	 ((? number?)    x)

	 ((? symbol?)    `(ncml:id ,x))

	 (('let bnds expr)
	  `(ncml:let (ncml:bnds . ,(map binding->ncml-binding bnds)) 
		     (ncml:expr ,(expr->ncml-expr expr))))

	 (((and op (? symbol?)) . args)
	  (let ((ncml-expr (if (member op builtin-fns)
			       (cons (op->ncml-op op) (map expr->ncml-expr args))
			       `(ncml:apply (@ (id ,op)) ,@(map expr->ncml-expr args)))))
	    ncml-expr))

	 (else (error 'expr->ncml-expr "unknown expression " x))))

  

(define (op->ncml-op op)
  (case op
    ((+)  'ncml:sum)
    ((-)  'ncml:sub)
    ((*)  'ncml:mul)
    ((/)  'ncml:div)
    ((>)  'ncml:gt)
    ((<)  'ncml:lt)
    ((<=) 'ncml:lte)
    ((>=) 'ncml:gte)
    ((=)  'ncml:eq)
    (else  ($ (string-append "ncml:" (->string op))))))



(define (declaration->ncml parse-expr)
  (lambda (x)

    (match x
	 (((or 'label 'LABEL) (and id (? symbol?)) '= (and v (? symbol?)))
	  `(ncml:label (@ (id ,(->string id))) ,v))

	 (((or 'input 'INPUT) . lst)
	  (map (lambda (x) 
		 (match x
			((? symbol?) 
			 `(ncml:input (@ (name ,(->string x)))))
			((id1 (or 'as 'AS) x1) 
			 `(ncml:input (@ (name ,(->string id1)) (as ,(->string x1)))))
			((id1 (or 'from 'FROM) n1)
			 `(ncml:input (@ (name ,(->string id1)) (from ,(->string n1)))))
			((id1 (or 'as 'AS) x1 (or 'from 'FROM) n1)
			 `(ncml:input (@ (name ,(->string id1)) 
					 (as ,(->string x1)) (from ,(->string n1)))))))
	       lst))


	 (((or 'output 'OUTPUT) . (and lst (? (lambda (x) (every symbol? x)))))
	  (map  (lambda (x) `(ncml:output (@ (name ,(->string x))))) lst))


	 (((or 'const 'CONST) (and id (? symbol?)) '= expr)
	  `(ncml:const (@ (name ,(->string id))) (ncml:expr ,(expr->ncml-expr (parse-expr expr)))))


	 (((or 'reaction 'REACTION) ((and id (? symbol?)) . alst) )
	  (let ((trs     (lookup-def 'transitions alst))
		(initial (lookup-def 'initial alst))
		(open    (lookup-def 'open alst))
		(cons    (lookup-def 'conserve alst))
		(p       (lookup-def 'power alst)))
	    (let ((sxml-trs (append-map transition->ncml-transition trs)))
	      `(ncml:reaction (@ (id ,(->string id))) 
			      (ncml:open ,(if (list? open) 
					      (string-concatenate (intersperse (map ->string open) ",")) 
					      open))
			      ,(and initial `(ncml:initial ,(expr->ncml-expr (parse-expr initial))) )
			      ,(and cons `(ncml:conserve ,((conseq->ncml-conseq parse-expr) cons)) )
			      (ncml:transitions ,@sxml-trs)
			      (ncml:power ,(expr->ncml-expr (parse-expr p)))))))

	 (((or 't 'T 'transient) ((and id (? symbol?))) '= (and expr (? nemo:expr?) ) . rest)
	  (let ((trs     (lookup-def 'transitions rest))
		(initial (lookup-def 'initial rest))
		(asgn    (lookup-def 'onevent rest))
		(p       (lookup-def 'power rest))
                )
            `(ncml:transient (@ (id ,(->string id))) 
                             (ncml:expr ,(expr->ncml-expr (parse-expr expr)))
                             (ncml:onevent ,(expr->ncml-expr (parse-expr asgn)))
                             ,(and initial `(ncml:initial ,(expr->ncml-expr (parse-expr initial))) )
                             ,(and p `(ncml:power ,(expr->ncml-expr (parse-expr p))))
                             ))
          )

	 (((or 'd 'D) ((and id (? symbol?))) '= expr . rest)
	  (let ((initial (lookup-def 'initial rest)))
	    `(ncml:rate (@ (name ,(->string id)) )
			,(and initial `(ncml:initial ,(expr->ncml-expr (parse-expr initial))))
			(ncml:expr ,(expr->ncml-expr (parse-expr expr))))))
  
			    
	 (((and id (? symbol?)) '= expr . rest)
	  `(ncml:asgn (@ (name ,id)) (ncml:expr ,(expr->ncml-expr (parse-expr expr)))))
		       
	 (((or 'defun 'DEFUN 'fun 'FUN 'rel 'REL) (and id (? symbol?)) 
	   (and idlist (? (lambda (x) (every symbol? x)))) expr)
	  `(ncml:defun (@ (id ,x)) 
		       ,@(map (lambda (v) `(ncml:arg ,(->string v))) idlist)
		       (ncml:body ,(expr->ncml-expr (parse-expr expr)))))
	 
	 (((or 'component 'COMPONENT) ((or 'type 'TYPE) typ) ((or 'name 'NAME) name) . lst)
	  `(ncml:component (@ (name ,(->string name)) (type ,(->string typ)))
			   ,@(map (declaration->ncml parse-expr) lst)))
	 
	 (((or 'component 'COMPONENT) ((or 'type 'TYPE) typ)  . lst)  
	  `(ncml:component (@ (type ,(->string typ)))
			   ,@(map (declaration->ncml parse-expr) lst)))
	 
	 (((or 'component 'COMPONENT)  ((or 'name 'NAME) name) '= 
	   (and functor-name (? symbol?)) (and args (? list?)))
	  `(ncml:component (@ (name ,(->string name)) 
			      (functor-name ,(->string functor-name)))
			   ,@(map (declaration->ncml parse-expr) lst)))
         
         (else (error  'declarations->ncml "unknown declaration " x))

	 )))


(define (make-component->ncml dis model parse-expr)
  (lambda (x) 
    (let ((en (hash-table-ref model x)))
	(cond ((procedure? en)
	       (let ((fd (procedure-data en)))
		 `(ncml:defun (@ (id ,x)) 
			      ,@(map (lambda (v) `(ncml:arg ,v)) (lookup-def 'vars fd))
			      (ncml:body ,(expr->ncml-expr (lookup-def 'body fd))))))
	      (else
	       (match en
		      (($ nemo:quantity 'LABEL  v) 
		       `(ncml:label (@ (id ,name)) ,v))
		      
		      (($ nemo:quantity 'EXTERNAL   local-name name namespace u)
		       (if namespace
			   `(ncml:input (@ (name ,name)) (as ,local-name) (from ,namespace))
			   `(ncml:input (@ (name ,name)) (as ,local-name))))

		      (($ nemo:quantity 'CONST  name value) 
		       `(ncml:const (@ (name ,name)) (ncml:expr ,value)))
		      
		      (($ nemo:quantity 'ASGN name value rhs)
		       (let ((expr (expr->ncml-expr rhs)))
			 `(ncml:asgn (@ (name ,name)) (ncml:expr ,expr))))
		      
		      (($ nemo:quantity 'RATE name initial rhs power u)
		       (let ((expr (expr->ncml-expr rhs))
			     (initial (and initial (expr->ncml-expr initial))))

			 `(ncml:rate (@ (name ,name)) 
				     ,(and initial `(ncml:initial ,initial))
				     (ncml:expr ,expr)
				     (ncml:power ,(or (and power (expr->ncml-expr power)) 
						      (expr->ncml-expr 1.0)))
				     )))
		      
		      (($ nemo:quantity 'TRANSIENT name initial rhs asgn power u)
		       (let ((expr (expr->ncml-expr rhs))
                             (asgn  (expr->ncml-expr asgn))
			     (initial (and initial (expr->ncml-expr initial))))

			 `(ncml:transient (@ (id ,name)) 
                                          ,(and initial `(ncml:initial ,initial))
                                          (ncml:expr ,expr)
                                          (ncml:onevent ,asgn)
                                          (ncml:power ,(or (and power (expr->ncml-expr power)) 
                                                           (expr->ncml-expr 1.0)))
                                          )))
		      
		      (($ nemo:quantity 'REACTION name initial open trs cons p u) 
		       (let ((sxml-trs (append-map transition->ncml-transition trs)))
			 `(ncml:reaction (@ (id ,name))
					 (ncml:open ,(if (list? open) 
							 (string-concatenate (intersperse (map ->string open) ",")) 
							 open))
					 ,(and initial `(ncml:initial ,(expr->ncml-expr initial)))
					 ,(and cons `(ncml:conserve ,(map (conseq->ncml-conseq identity) cons)) )
					 (ncml:transitions ,@sxml-trs)
					 (ncml:power ,(expr->ncml-expr p)))))
		      
		      (($ nemo:quantity 'COMPONENT name type lst) 
		       (let ((component->ncml (make-component->ncml dis model parse-expr))
			     (component-exports ((dis 'component-exports) model x)))
			 (case type
			   ((toplevel) `(,@(map component->ncml lst)
					 ,@(map (lambda (x) `(ncml:output (@ (name ,x)))) component-exports)))
			   (else `(ncml:component (@ (name ,name) (type ,type))
						  ,@(filter-map component->ncml lst)
						  ,@(map (lambda (x) `(ncml:output (@ (name ,x)))) component-exports)
						  )))))
		      
		      (($ nemo:quantity 'FUNCTOR name args type lst) 
		       (let ((component->ncml (make-component->ncml dis model parse-expr)))
			 `(ncml:functor (@ (name ,name) (type ,type) 
					   (parameters ,(string-intersperse (map ->string args) ",")))
					,@(filter-map (declaration->ncml parse-expr) lst)
					)))
		      
		      (else #f)))))))
    

(define (model->ncml model parse-expr)
  (match-let ((($ nemo:quantity 'DISPATCH  dis)     
	       (hash-table-ref model (nemo-intern 'dispatch))))
     (let ((sysname     ((dis 'sysname) model))
	   (component->ncml (make-component->ncml dis model parse-expr)))
       `(ncml:model (@ (name ,sysname)) ,@(component->ncml (nemo-intern 'toplevel))))))
	   

(define (transition->text-transition x)
  (match x
	 (('-> src dst rate) 
	  `(-> ,src ,dst ,(expr->text-expr rate) ))
	 ((src '-> dst rate) 
	  `(-> ,src ,dst ,(expr->text-expr rate) ))
	 (('<-> src dst rate1 rate2) 
	  `(<-> ,src ,dst ,(expr->text-expr rate) ))
	 (('src <-> dst rate1 rate2) 
	  `(<-> ,src ,dst ,(expr->text-expr rate) ))
	 (else (error 'transition->text-transition "invalid transition " x))))


(define (conseq->text-conseq parse-expr)
  (lambda (x)
    (match x 
	   (((and i (? integer?)) '= rhs)
	    `(,(->string i) =
	      ,(expr->text-expr (parse-expr rhs))))
	   (else (error 'conseq->text-conseq "invalid linear equation " x)))))


(define (binding->text-binding bnd)
  (match bnd
	 ((id expr)  `(,id = ,(expr->text-expr expr)))
	 (else (error 'binding->text-binding "invalid binding " bnd))))

  
(define (expr->text-expr x)
  (match x
	 ((? number?)    x)
	 ((? symbol?)    x)
	 (('let bnds expr)
	  `(let (,(map binding->text-binding bnds))
	     ,(expr->text-expr expr)))
	 (((and op (? symbol?)) . args)
	  (let ((ncml-expr `(apply ,op ,@(map expr->text-expr args))))
	    ncml-expr))
	 (else (error 'expr->text-expr "unknown expression " x))))


(define (make-component->text dis model parse-expr)
  (lambda (x) 
    (let ((en (hash-table-ref model x)))
	(cond ((procedure? en)
	       (let ((fd (procedure-data en)))
		 `(function ,x
			    ,(lookup-def 'vars fd) =
			    ,(expr->text-expr (lookup-def 'body fd)))
		 ))
	      (else
	       (match en
		      (($ nemo:quantity 'LABEL  v) 
		       `(label ,name = ,v))
		      
		      (($ nemo:quantity 'EXTERNAL local-name name namespace u)
		       (if namespace
			   `(input ,name  as ,local-name from ,namespace)
			   `(input ,name  as ,local-name)))

		      (($ nemo:quantity 'CONST  name value) 
		       `(const ,name = ,value))
		      
		      (($ nemo:quantity 'ASGN name value rhs)
		       (let ((expr (expr->text-expr rhs)))
			 `(,name = ,expr)))
		      
		      (($ nemo:quantity 'RATE name initial rhs power u)
		       (let ((expr (expr->ncml-expr rhs))
			     (initial (and initial (expr->text-expr initial)))
			     (power (or (and power (expr->text-expr power))
					(expr->text-expr 1.0))))

			 `(d (,name) = (,expr)
			     (initial: ,initial)
			     (power: ,power))
			 ))

		      
		      (($ nemo:quantity 'REACTION name initial open trs cons p u) 
		       (let ((sxml-trs (append-map transition->text-transition trs)))
			 `(reaction  ,name
				     (open-state: ,open) 
				     (initial: ,(expr->text-expr initial))
				     (conserve: ,(map (conseq->text-conseq identity) cons))
				     (transitions: ,text-trs)
				     (power: ,(expr->ncml-expr p))
				     )))

		      
		      (($ nemo:quantity 'COMPONENT name type lst) 
		       (let ((component->text (make-component->text dis model parse-expr))
			     (component-exports ((dis 'component-exports) model x)))
			 (case type
			   ((toplevel) `(,@(map component->text lst)
					 ,@(map (lambda (x) `(output ,x)) component-exports)))
			   (else `(component ,name (type: ,(->string type) )
						  ,@(filter-map component->text lst)
						  ,@(map (lambda (x) `(output ,x)) component-exports)
						  )))))
		      
		      (($ nemo:quantity 'FUNCTOR name args type lst) 
		       (let ((component->ncml (make-component->ncml dis model parse-expr)))
			 `(functor ,name (type: ,(->string type) )
				   (parameters: ,(string-intersperse (map ->string args) ","))
				   ,@(filter-map (declaration->ncml parse-expr) lst)
				   )))
		      
		      (else #f)))
	      ))
    ))
    

(define (model->text model parse-expr)
  (match-let ((($ nemo:quantity 'DISPATCH  dis)     
	       (hash-table-ref model (nemo-intern 'dispatch))))
     (let ((sysname     ((dis 'sysname) model))
	   (component->text (make-component->text dis model parse-expr)))
       `(model ,sysname ,@(component->text (nemo-intern 'toplevel)))
       )))
	   

(include "expr-parser.scm")
(include "SXML.scm")
(include "SXML-to-XML.scm")
(include "stx-engine.scm")



(define (ensure-xmlns doc)
  (let ((doc1 (sxml:add-attr doc '(xmlns:ncml "ncml"))))
    (sxml:add-attr doc1 '(xmlns ncml))))


;; based on SRV:send-reply by Oleg Kiselyov
(define (print-fragments b)
  (let loop ((fragments b) (result #f))
    (cond
      ((null? fragments) result)
      ((not (car fragments)) (loop (cdr fragments) result))
      ((null? (car fragments)) (loop (cdr fragments) result))
      ((eq? #t (car fragments)) (loop (cdr fragments) #t))
      ((pair? (car fragments))
        (loop (cdr fragments) (loop (car fragments) result)))
      ((procedure? (car fragments))
        ((car fragments))
        (loop (cdr fragments) #t))
      (else
       (display (car fragments))
       (loop (cdr fragments) #t)))))


(define (ncml->declarations ncml:model parse-expr)
  (letrec
       ((label-template 
	(sxml:match 'ncml:label
		    (lambda (node bindings root env) 
		      (let ((id   (or (sxml:attr node 'id) (sxml:attr node 'name)))
			    (v    (or (sxml:attr node 'value)
				      (sxml:text node)))
                            )
			(if (not id) (error 'output-template "label declaration requires id attribute"))
			`(label ,($ id) = ,($ v))
                        ))
                    ))
       
	(input-template 
         (sxml:match 'ncml:input
                     (lambda (node bindings root env) 
                       (let ((id    (or (sxml:attr node 'id) (sxml:attr node 'name)))
                             (from  (sxml:attr node 'from))
                             (as    (sxml:attr node 'as))
                             (unit  (sxml:attr node 'unit))
                             )
                         (if (not id) (error 'input-template "input declaration requires id attribute"))
                         (cond ((and from as unit)
                                `(input (,($ id) as ,($ as ) from ,($ from) (unit ,($ unit)))))
                               ((and from as)
                                `(input (,($ id) as ,($ as ) from ,($ from) )))
                               ((and from unit)
                                `(input (,($ id) from ,($ from) (unit ,($ unit)))))
                               (from
                                `(input (,($ id) from ,($ from))))
                               (as             
                                `(input (,($ id) as ,($ as))))
                               ((and as unit)
                                `(input (,($ id) as ,($ as) (unit ,($ unit)))))
                               (else           
                                `(input ,($ id))))
                         ))
                     ))
       
       (output-template 
	(sxml:match 'ncml:output
		    (lambda (node bindings root env) 
		      (let ((id   (or (sxml:attr node 'id)
				      (sxml:attr node 'name))))
			(if (not id) (error 'output-template "output declaration requires id attribute"))
			`(output ,($ id))
                        ))
                    ))
       
       (const-template 
	(sxml:match 'ncml:const
		    (lambda (node bindings root env) 
		      (let* ((unit (sxml:attr node 'unit))
                             (id   (or (sxml:attr node 'id)
				       (sxml:attr node 'name)))
			     (expr ((lambda (x) 
				      (if (not x)  
					  (error 'const-template "const declaration " id " requires expr element")
					  (parse-expr (second x) id)))
				   (or (sxml:kidn* 'ncml:expr node)
				       (let ((vattr (sxml:attr node 'value)))
					 (and vattr (list 'value vattr )))
				       (list 'value (sxml:text node))
				       )
				   )))
			(if (not id) (error 'const-template "const declaration requires id attribute"))
                        (if unit
                            `(const ,($ id) = ,expr (unit ,($ unit)))
                            `(const ,($ id) = ,expr)
                            )
                        ))
                    ))
       
       (reaction-transition-template 
	(sxml:match 'ncml:transition
		    (lambda (node bindings root env) 
		      (let (
                            (src  (sxml:attr node 'src))
			    (dst  (sxml:attr node 'dst))
			    (rate  ((lambda (x) 
				      (if (not x)  
					  (error 'reaction-transition-template 
						 "reaction transition requires rate element")
					  (parse-expr (second x))))
				    (sxml:kidn* 'ncml:rate node))))
			(if (not src) (error 'reaction-transition-template
					     "reaction transition requires src attribute"))
			(if (not dst) (error 'reaction-transition-template
					     "reaction transition requires dst attribute"))
			
			`(-> ,($ src) ,($ dst) ,rate)))
                    ))
       
       (asgn-template 
	(sxml:match 'ncml:asgn
		    (lambda (node bindings root env) 
		      (let* ((unit (sxml:attr node 'unit))
                             (id   (or (sxml:attr node 'id) (sxml:attr node 'name)))
			     (expr ((lambda (x) 
				      (if (not x)  
					  (error 'asgn-template "algebraic assignment requires expr element")
					  (parse-expr (second x) id)))
				    (or (sxml:kidn* 'ncml:expr node)
					(list 'expr (sxml:text node))
					))
				   )
                             )
			(if (not id) (error 'asgn-template "algebraic assignment requires id attribute"))
                        (if unit
                            `(,($ id) = ,expr)
                            `(,($ id) = ,expr (unit ,($ unit))))
                        ))
                    ))
       
       (rate-template 
	(sxml:match 'ncml:rate
		    (lambda (node bindings root env) 
		      (let* ((unit (sxml:attr node 'unit))
                             (id   (or (sxml:attr node 'id) (sxml:attr node 'name)))
			     (rhs  ((lambda (x) 
                                      (if (not x)  
                                          (error 'rate-template "rate equation requires expr element")
					  (parse-expr (second x) id)))
                                    (sxml:kidn* 'ncml:expr node)))
			     (initial ((lambda (x) (and x (parse-expr (second x) id)))
				       (sxml:kidn* 'ncml:initial node)))
			     (power ((lambda (x) (and x (parse-expr (second x) id)))
				     (sxml:kidn* 'ncml:power node)))
			     )
			(if (not id) (error 'rate-template "rate equation requires id attribute"))
                        (cond
                         ((and power initial unit)
                          `(d (,($ id)) = ,rhs  (initial ,initial) (power ,power) (unit ,($ unit))))
                         ((and power initial)
                          `(d (,($ id)) = ,rhs  (initial ,initial) (power ,power)))
                         ((and power unit)
                          `(d (,($ id)) = ,rhs  (power ,power) (unit ,($ unit))))
                         ((and initial unit)
                          `(d (,($ id)) = ,rhs  (initial ,initial) (unit ,($ unit))))
                         (initial
                          `(d (,($ id)) = ,rhs  (initial ,initial)))
                         (power
                          `(d (,($ id)) = ,rhs  (power ,power)))
                         (unit
                          `(d (,($ id)) = ,rhs (unit ,($ unit))))
                         (else
                          `(d (,($ id)) = ,rhs))
                         )
                        ))
                    ))
       
       (transient-template 
	(sxml:match 'ncml:transient
		    (lambda (node bindings root env) 
		      (let* ((unit  (sxml:attr node 'unit))
                             (id    (or (sxml:attr node 'id) (sxml:attr node 'name)))
			     (rhs   ((lambda (x) 
                                       (if (not x)  
                                           (error 'rate-template "rate equation requires expr element")
                                           (parse-expr (second x) id)))
                                     (sxml:kidn* 'ncml:expr node)))
			     (initial ((lambda (x) (and x (parse-expr (second x) id)))
				       (sxml:kidn* 'ncml:initial node)))
			     (onevent ((lambda (x) (and x (parse-expr (second x) id)))
				       (sxml:kidn* 'ncml:onevent node)))
			     (power ((lambda (x) (and x (parse-expr (second x) id)))
				     (sxml:kidn* 'ncml:power node)))
			     )
			(if (not id) (error 'transient-template "transient equation requires id attribute"))
                        (cond
                         ((and power initial unit)
                          `(transient (,($ id)) = ,rhs (onevent ,onevent) 
                                      (initial ,initial) (power ,power) (unit ,($ unit))))
                         ((and power initial)
                          `(transient (,($ id)) = ,rhs (onevent ,onevent) 
                                      (initial ,initial) (power ,power)))
                         ((and power unit)
                          `(transient (,($ id)) = ,rhs (onevent ,onevent) 
                                      (power ,power) (unit ,($ unit))))
                         ((and initial unit)
                          `(transient (,($ id)) = ,rhs (onevent ,onevent) 
                                      (initial ,initial) (unit ,($ unit))))
                         (initial
                          `(transient (,($ id)) = ,rhs (onevent ,onevent) 
                                      (initial ,initial)))
                         (power
                          `(transient (,($ id)) = ,rhs (onevent ,onevent) 
                                      (power ,power)))
                         (unit
                          `(transient (,($ id)) = ,rhs (onevent ,onevent) 
                                      (unit ,($ unit))))
                         (else 
                          `(transient (,($ id)) = ,rhs (onevent ,onevent) ))
                         ))
                      ))
        )

       (conseq-template 
	(sxml:match 'ncml:conseq
		    (lambda (node bindings root env) 
		      (let ((val   (string->number (->string (sxml:attr node 'val))))
			    (rhs   ((lambda (x) 
					(if (not x)  
					    (error 'conseq-template 
						   "conseq definition requires expr element")
					    (parse-expr (second x))))
				      (sxml:kidn* 'ncml:expr node))))
			`(,val = ,rhs)))
                    ))
       
       (reaction-template 
	(sxml:match 'ncml:reaction
		    (lambda (node bindings root env) 
		      (let* ((unit    (sxml:attr node 'unit))
                             (id      ($ (or (sxml:attr node 'id) (sxml:attr node 'name))))
                             (initial ((lambda (x) (and x (parse-expr (second x) id)))
                                       (sxml:kidn* 'ncml:initial node)))
                             
                             (open    ((lambda (x) 
					(if (not x)  
					    (error 'reaction-template
						   "reaction declaration requires open element")
					    (let ((os (string-split (second x) ",")))
					      (map $ os))))
                                       (sxml:kidn* 'ncml:open node)))
                             
                             (conserve ((lambda (x) 
                                          (and x (let ((tmpl (sxml:make-null-ss conseq-template)))
                                                   (stx:apply-templates (cdr x) tmpl root env))))
                                        (sxml:kidn* 'ncml:conserve node)))
                             
                             (power ((lambda (x) 
                                       (if (not x)  
                                           (error 'reaction-template
                                                  "reaction declaration requires open element")
                                           (parse-expr (second x) id)))
                                     (sxml:kidn* 'ncml:power node)))
                             
                             (transitions ((lambda (x) 
                                             (if (not x)  
                                                 (error 'reaction-template
                                                        "reaction declaration requires transitions element")
                                                 (let ((tmpl (sxml:make-null-ss reaction-transition-template)))
                                                   (stx:apply-templates (cdr x) tmpl root env))))
                                           (sxml:kidn* 'ncml:transitions node)))
                             
			    )
					  
			(if (not id) (error 'reaction-template "reaction declaration requires id attribute"))
                        (cond
                         ((and conserve unit)
                          `(reaction (,id (initial ,initial) (open ,open) (power ,power) 
                                          (conserve ,conserve)
                                          (transitions ,@transitions)
                                          (unit ,($ unit)))))
                         (conserve
                          `(reaction (,id (initial ,initial) (open ,open) (power ,power) 
                                          (conserve ,conserve)
                                          (transitions ,@transitions))))

                         (unit
                          `(reaction (,id (initial ,initial) (open ,open) (power ,power) 
                                          (transitions ,@transitions)
                                          (unit ,($ unit)))))
                         ))
                      ))
        )

       (defun-template 
	(sxml:match 'ncml:defun
		    (lambda (node bindings root env) 

		      (let* ((id    (or (sxml:attr node 'id) (sxml:attr node 'name)))
			     (args  ((lambda (x) 
				       (if (null? x)  
					   (error 'defun-template 
						  "function definition requires at least one arg element")
					   (map (compose $ second) x)))
				     (sxml:kidsn 'ncml:arg node)))
			     (body ((lambda (x) 
				      (if (not x)  
					  (error 'defun-template
						 "function definition requires body element")
					  (parse-expr (second x) id)))
				    (sxml:kidn* 'ncml:body node))))
			(if (not id) (error 'defun-template "function definition requires id attribute"))

			`(defun ,($ id) ,args ,body)))))

       (component-template
	(sxml:match 'ncml:component
		    (lambda (node bindings root env)

		      (let ((name (sxml:attr node 'name))
			    (functor-name (or (sxml:attr node 'functor-name)
					      (sxml:attr node 'functor)))
			    (type (sxml:attr node 'type)))

			(if (and (not functor-name) (not type) )
			    (error 'component-template "component definition requires type attribute" name))
			(if (and functor-name (not name) )
			    (error 'component-template "component definition requires name attribute"))
			(if functor-name
			    `(component (name ,($ name)) = ,($ functor-name) 
					,(ncml->declarations (sxml:kids node) parse-expr))
			    (if name
				`(component (type ,($ type)) (name ,($ name)) 
					    ,@(ncml->declarations (sxml:kids node) parse-expr))
				`(component (type ,($ type)) 
					    ,@(ncml->declarations (sxml:kids node) parse-expr))
				))
			))
		    ))

       (functor-template
	(sxml:match 'ncml:functor
		    (lambda (node bindings root env)

		      (let ((parameters (sxml:attr node 'parameters))
			    (name (sxml:attr node 'name))
			    (type (sxml:attr node 'type)))
			(if (not type) (error 'functor-template "functor definition requires type attribute"))
			(if (not name) (error 'functor-template "functor definition requires name attribute"))
			(if (not parameters) 
			    (error 'functor-template "functor definition requires parameters attribute"))
			`(functor (name ,($ name)) (type ,($ type)) 
				  ,(map $ (string-split parameters ","))
				  = . ,(ncml->declarations (sxml:kids node) parse-expr))))))

       (hh-template 
	(sxml:match 'ncml:hh_ionic_gate 
		    (lambda (node bindings root env)
		      (let* (
			     (id         (or (sxml:attr node 'id) (sxml:attr node 'name)))
			     (and-expr   (lambda (x) (and x (parse-expr (second x) id))))
			     (initial_m  (and-expr (sxml:kidn* 'ncml:initial_m node)))
			     (initial_h  (and-expr (sxml:kidn* 'ncml:initial_h node)))
			     (m_power    (and-expr (sxml:kidn* 'ncml:m_power node)))
			     (h_power    (and-expr (sxml:kidn* 'ncml:h_power node)))
			     (m_alpha    (and-expr (sxml:kidn* 'ncml:m_alpha node)))
			     (m_beta     (and-expr (sxml:kidn* 'ncml:m_beta node)))
			     (h_alpha    (and-expr (sxml:kidn* 'ncml:h_alpha node)))
			     (h_beta     (and-expr (sxml:kidn* 'ncml:h_beta node)))
			     (m_tau      (and-expr (sxml:kidn* 'ncml:m_tau node)))
			     (m_inf      (and-expr (sxml:kidn* 'ncml:m_inf node)))
			     (h_tau      (and-expr (sxml:kidn* 'ncml:h_tau node)))
			     (h_inf      (and-expr (sxml:kidn* 'ncml:h_inf node)))
			     )

			(if (not id)
			    (error 'hh-template "hh ionic conductance definition requires id attribute"))
			`(hh-ionic-gate 
			  (,($ id)
			   ,@(if initial_m `((initial-m ,initial_m)) `())
			   ,@(if initial_h `((initial-h ,initial_h)) `())
			   ,@(if m_power `((m-power ,m_power)) '())
			   ,@(if h_power `((h-power ,h_power)) '())
			   ,@(if m_alpha `((m-alpha ,m_alpha)) '())
			   ,@(if h_alpha `((h-alpha ,h_alpha)) '())
			   ,@(if m_beta  `((m-beta ,m_beta)) '())
			   ,@(if h_beta  `((h-beta ,h_beta)) '())
			   ,@(if m_inf   `((m-inf ,m_inf)) '())
			   ,@(if h_inf   `((h-inf ,h_inf)) '())
			   ,@(if m_tau   `((m-tau ,m_tau)) '())
			   ,@(if h_tau   `((h-tau ,h_tau)) '())
			   ))))))

       (decaying-pool-template 
	(sxml:match 'ncml:decaying_pool 
		    (lambda (node bindings root env)
		      (let* ((id         (sxml:attr node 'id))
			     (and-expr   (lambda (x) (and x (parse-expr (second x) id))))
			     (initial    (and-expr (sxml:kidn* 'ncml:initial node)))
			     (beta       (and-expr (sxml:kidn* 'ncml:beta node)))
			     (depth      (and-expr (sxml:kidn* 'ncml:depth node)))
			     (temp-adj   (and-expr (sxml:kidn* 'ncml:temp_adj node))))
			(if (not id)
			    (error 'decaying-pool-template "decaying pool definition requires id attribute"))
			(if (not initial) 
			    (error 'decaying-pool-template "decaying pool definition requires initial value"))
			(if (not beta) 
			    (error 'decaying-pool-template "decaying pool definition requires beta parameter"))
			(if (not depth) 
			    (error 'decaying-pool-template "decaying pool definition requires depth parameter"))
			    
			`(decaying-pool 
			  (,($ id)
			   ,@(if temp_adj `((temp_adj ,temp_adj)) `())
			   (beta ,beta)
			   (depth ,depth)
			   (initial ,initial)))))))
	)

     (stx:apply-templates 
      ncml:model 
      (sxml:make-null-ss label-template
                         input-template
                         output-template
                         const-template
                         asgn-template
                         rate-template
                         reaction-template
                         transient-template
                         defun-template
                         component-template
                         functor-template
                         hh-template
                         decaying-pool-template) 
      ncml:model (list))

     ))


(define sxslt-preamble
  `(
    (import scheme chicken)
    (require-extension sxml-transforms sxpath sxpath-lolevel) 
    (define-syntax  sxml:match
      (syntax-rules  ()
	((match pattern handler)
	 (list (if (symbol? pattern) pattern (sxpath pattern))
	       handler))
	))
    (define identity-template 
      `(*default* ,(lambda (node bindings root env) 
		     (begin 
		       node))))
    (define-syntax sxml:make-ss
      (syntax-rules  ()
	((stx rule ...)
	 (list 
	  identity-template
	  (list '*text*  (lambda (text) text)) 
	  rule ...))
	))
    (define (sxml:kid node)
      (let ((v ((select-first-kid
		 (lambda (x) (not (eq? (car x) '@)))) node)))
	(if (not v)
	    (error 'sxml:kid "node does not have children" node)  v)))
    (define (sxml:kids node)
      ((select-kids (lambda (x) (not (eq? (car x) '@)))) node))
    (define (sxml:kidsn name node)
      ((select-kids (lambda (x) (eq? (car x) name))) node))
    (define (sxml:kidn name node)
      ((select-first-kid (lambda (x)  (eq? (car x) name))) node))  
    ))


(define (ncml->model-decls options doc)

  (define (load-ss in)
    (eval `(begin
             ,@sxslt-preamble
             (sxml:make-ss ,@(read in))
             )))

  (define (make-ss-fname dirname fname) 
    (or (and dirname (make-pathname dirname fname)) fname))

  (let* ((source-path   (lookup-def 'source-path options))
         (dirname       (pathname-directory source-path))
         (parse-expr    (or (lookup-def 'parse-expr options) identity))
	 (ncml:model    ((lambda (x) 
			   (if (null? x) (error 'ncml->model "ncml:model element not found in input document") (car x)))
			 (ncml:sxpath '(// ncml:model) `(*TOP* . ,doc))))
	 (model-name     ($ (or (sxml:attr ncml:model 'name) (gensym 'model))))
	 (membraneprops  (ncml:sxpath '(// cell biophysicalProperties membraneProperties) 
				      `(*TOP* . ,ncml:model)))
	 (ncml-ss        (ncml:sxpath '(// ncml:sxslt) `(*TOP* . ,ncml:model)))
	 (ncml-templates (ncml:sxpath '(// ncml:template) `(*TOP* . ,ncml:model)))
	 (ncml-decls     ((lambda (doc) 
			    (if (null? ncml-ss) doc
				(let ((ss (map 
					   (lambda (x)
                                             (let ((fn (sxml:attr x 'filename)))
                                               (or (and fn (call-with-input-file (make-ss-fname dirname fn) load-ss))
                                                   (call-with-input-string (sxml:text x) load-ss))
                                               ))
					   ncml-ss)))
				  (fold (lambda (s doc) (stx:apply-templates doc s doc (list))) doc ss))
				))
			  (if (null? membraneprops) 
			      (sxml:kids ncml:model) 
			      (sxml:kids membraneprops))))
	 (dd  (if (lookup-def 'debug options)
		  (begin (pp ncml-decls))))
	 (model-decls    (ncml->declarations ncml-decls parse-expr))
	 (user-templates (map (lambda (t)
				 (let ((name (or (sxml:attr t 'name) (->string (gensym 'template))))
				       (args (or (let ((xs (sxml:attr t 'args)))
						   (or (and xs (string-split xs ",")) '())))))
				   (list name args (read-template (sxml:text t)))
				   ))
			       ncml-templates))
	 )
    (list model-name model-decls user-templates)))


(define (ncml-model-decls->model options model-name model-decls)

    (if (or (null? model-decls)  (and (pair? model-decls) (every null? model-decls)))
	(error 'ncml-model-decls->model "ncml declaration elements not found in input document"))

    (let* ((model+nemo  (nemo-constructor model-name model-decls (lambda (x . rest) (identity x))))
	   (model       (first model+nemo))
	   (nemo        (second model+nemo)))

      (let ((model-1 (nemo:hh-transformer model (alist-ref 'hh-markov options) (lambda (x . rest) (identity x)))))

	(if (assoc 'depgraph options) (print "dependency graph: " ((nemo 'depgraph*) model-1)))
	(if (assoc 'exports options) (print "exports: " ((nemo 'exports) model-1)))	
	(if (assoc 'imports options) (print "imports: " ((nemo 'imports) model-1)))
	(if (assoc 'components options)
	    (for-each (lambda (x) 
			(print "component " x ": " ((nemo 'component-exports) model-1 (second x)))
			(print "component " x " subcomponents: " ((nemo 'component-subcomps) model-1 (second x))))
		      ((nemo 'components) model-1)))
	model-1)))



(define nineml-reaction-transition-template 
  (sxml:match 'nmlb:transition
              (lambda (node bindings root env) 
                (let (
                      (src  (sxml:attr node 'src))
                      (dst  (sxml:attr node 'dst))
                      (rate  ((lambda (x) 
                                (if (not x)  
                                    (error 'nineml-reaction-transition-template 
                                           "reaction transition requires rate element")
                                    (sxml:text x)))
                              (sxml:kidn* 'nmlb:rate node)))
                      )
                  (if (not src) (error 'nineml-reaction-transition-template
                                       "reaction transition requires src attribute"))
                  (if (not dst) (error 'nineml-reaction-transition-template
                                       "reaction transition requires dst attribute"))
                  
                  `(ncml:transition (@ (src ,src) (dst ,dst)) (ncml:rate ,rate))
                  ))
              ))



(define nineml-ss

  (sxml:make-null-ss 

   (sxml:match 'nmlb:ComponentClass 
               (lambda (node bindings root env) 
                 (let* ((name       (sxml:attr node 'name))
                        (type       (sxml:attr node 'type))
                        (interface  (sxml:kidn 'nmlb:Interface node))
                        (parameters (let ((xs (sxml:kidsn 'nmlb:Parameter interface)))
                                      (filter-map (lambda (x) (sxml:attr x 'name)) xs)))
                        (decls      (let ((xs (sxml:kids node)))
                                      (filter (lambda (x) 
                                                (not (equal? (sxml:element-name x) 'nmlb:Interface)))
                                              xs)))
                        )
                   (let ((decls1 (stx:apply-templates decls nineml-ss root env)))
                     `(ncml:functor (@ (type ,type) (name ,name)
                                       (parameters ,(slp "," parameters)))
                                    . ,decls1) )
                   )))


   (sxml:match 'nmlb:Component

               (lambda (node bindings root env) 

                 (define (read-xml-string s)
                          (call-with-input-string s
                            (lambda (port) 
                              (ssax:xml->sxml port
                                              '((nmlb . "http://www.nineml.org/Biophysics")))
                              )))

                 (let ((name       (sxml:attr node 'name))
                       (type       (sxml:attr node 'type))
                       (definition (sxml:attr node 'definition))
                       (definition-uri (let ((link (or (sxml:kidn 'nmlb:link node)
                                                       (sxml:kidn 'nmlb:url node))))
                                         (and link (uri-reference link))))
                       (decls      (sxml:kids node))
                       (properties (map cdr (sxml:kidsn 'nmlb:properties node)))
                       )

                   (let (
                         (parameters (stx:apply-templates 
                                      (concatenate properties)
                                      nineml-ss
                                      root env))
                         )

                     (cond (definition-uri
                             (let* (
                                    (definition-str (nemo:fetch definition-uri))
                                    (definition-sxml (read-xml-string definition-str))
                                    (definition (stx:apply-templates definition-sxml nineml-ss root env))
                                    )
                               (case (car definition) 
                                 ((ncml:functor)
                                  (let* (
                                         (definition-attrs (alist-ref '@ definition))
                                         (definition-name (alist-ref 'name definition-attrs))
                                         )
                                    (env (cons `(,definition-name . ,definition) (env)))
                                    `(ncml:component (@ (name ,name) (functor-name ,definition-name) )
                                                     . ,parameters))
                                  )
                                 (else (error 'nineml->model-decls 
                                              "unable to find component class in definition uri " 
                                              definition-uri))
                                 ))
                             )
                            (definition
                              `(ncml:component (@ (name ,name) (functor-name ,definition) )
                                               . ,parameters))
                            (else
                             (let ((decls1 (stx:apply-templates decls nineml-ss root env)))
                               (cond ((and name type)
                                      `(ncml:component (@ (name ,name) (type ,type) )
                                                       . ,(append parameters decls1)))
                                     (type
                                      `(ncml:component (@ (type ,type) )
                                                       . ,(append parameters decls1)))
                                     (else
                                      `(ncml:component . ,(append parameters decls1)))))
                             ))
                     ))
                 ))
               

  (sxml:match 'nmlb:Parameter
              (lambda (node bindings root env) 
                (let (
                      (name  (sxml:attr node 'name))
                      (value (sxml:text (sxml:kidn* 'nmlb:value node)))
                      (unit  (sxml:kidn 'nmlb:unit node))
                      )
                  `(ncml:const (@ (name ,name) ,@(if unit `((unit ,(sxml:text unit))) `())) (ncml:expr ,value) )
                  ))
              )

   (sxml:match 'nmlb:Alias
               (lambda (node bindings root env) 
                 (let ((name       (sxml:attr node 'name))
                       (arguments  (string-split 
                                    (or (sxml:attr node 'argument)
                                        (sxml:attr node 'arguments)) ","))
                       )
                   `(ncml:defun (@ (name ,name))
                                ,@(map (lambda (x) `(ncml:arg ,x)) arguments)
                                (ncml:body ,(sxml:text (sxml:kid node)))
                                ))
                 ))

   (sxml:match 'nmlb:AnalogPort
               (lambda (node bindings root env) 
                 (let ((name (sxml:attr node 'name))
                       (mode (sxml:attr node 'mode))
                       (from (sxml:attr node 'from))
                       (unit (sxml:kidn 'nmlb:unit node))
                       )
                   (cond
                    ((string=? mode "receive")
                     (if from
                         `(ncml:input  (@ (name ,name) (from ,from) ,@(if unit `((unit ,(sxml:text unit))) `())))
                         `(ncml:input  (@ (name ,name) ,@(if unit `((unit ,(sxml:text unit))) `())))
                         ))
                    ((string=? mode "send")
                     `(ncml:output  (@ (name ,name) ,@(if unit `((unit ,(sxml:text unit))) `()))))

                    (error 'nineml->model-decls "unknown analog port mode" mode))
                   )
                 ))

   (sxml:match 'nmlb:Assignment
               (lambda (node bindings root env) 
                 (let (
                       (name (sxml:attr node 'name))
                       (rhs  (sxml:text (sxml:kidn* 'nmlb:rhs node)))
                       (unit (sxml:kidn 'nmlb:unit node))
                       )
                   `(ncml:asgn (@ (name ,name) ,@(if unit `((unit ,(sxml:text unit))) `()))
                               (ncml:expr ,rhs))
                   ))
               )

   (sxml:match 'nmlb:TimeDerivative
               (lambda (node bindings root env) 
                 (let (
                       (name    (sxml:attr node 'variable))
                       (rhs     (sxml:kidn* 'nmlb:rhs node))
                       (initial (sxml:kidn* 'nmlb:initial node))
                       (unit    (sxml:kidn 'nmlb:unit node))
                       )
                   `(ncml:rate (@ (name ,name) )
                              (ncml:expr ,(sxml:text rhs))
                              ,@(if initial `((ncml:initial ,(sxml:text initial))) '())
                              ,@(if unit `((unit ,(sxml:text unit))) `())
                              )
                   ))
               )

   (sxml:match 'nmlb:Transient
               (lambda (node bindings root env) 
                 (let (
                       (name    (sxml:attr  node 'variable))
                       (onevent (sxml:kidn* 'nmlb:onevent node))
                       (power   (sxml:kidn* 'nmlb:power node))
                       (rhs     (sxml:kidn* 'nmlb:rhs node))
                       (initial (sxml:kidn* 'nmlb:initial node))
                       (unit    (sxml:kidn 'nmlb:unit node))
                       )
                   `(ncml:transient (@ (name ,name) )
                                   (ncml:expr ,(sxml:text rhs))
                                   (ncml:onevent ,(sxml:text onevent))
                                   ,@(if initial `((ncml:initial ,(sxml:text initial))) '())
                                   ,@(if power `((ncml:power ,(sxml:text power))) '())
                                   ,@(if unit `((unit ,(sxml:text unit))) `())
                                   )
                   ))
               )


   (sxml:match 'nmlb:Reaction
               (lambda (node bindings root env) 
                 (let (
                       (name     (sxml:attr  node 'variable))
                       (open     (sxml:text (sxml:kidn* 'nmlb:open node)))
                       (power    (sxml:text (sxml:kidn* 'nmlb:power node)))
                       (conserve (sxml:kidn* 'nmlb:conserve node))
                       (transitions (let ((tmpl (sxml:make-null-ss nineml-reaction-transition-template)))
                                      (stx:apply-templates (sxml:kids (sxml:kidn* 'nmlb:transitions node)) tmpl root env)))
                       (initial (sxml:kidn* 'nmlb:initial node))
                       (unit    (sxml:kidn 'nmlb:unit node))
                       )
                   `(ncml:reaction (@ (name ,name) )
                                   (ncml:transitions . ,transitions)
                                   (ncml:open ,open)
                                   ,(if conserve
                                        `(ncml:conserve
                                          (ncml:conseq (@ (val ,(sxml:attr conserve 'val)))
                                                       (ncml:expr ,(sxml:text conserve))))
                                        '())
                                   ,@(if initial `((ncml:initial ,initial)) '())
                                   ,@(if power   `((ncml:power ,power)) '())
                                   ,@(if unit    `((unit ,(sxml:text unit))) `())
                                   )
                   ))
               )

   
   (sxml:match 'nmlb:hh_ionic_gate 
               (lambda (node bindings root env)
                 (let* (
                        (id         (or (sxml:attr node 'id) (sxml:attr node 'name)))
                        (and-text   (lambda (x) (and x (sxml:text x))))
                        (initial_m  (and-text (sxml:kidn* 'nmlb:initial_m node)))
                        (initial_h   (and-text (sxml:kidn* 'nmlb:initial_h node)))
                        (m_power     (and-text (sxml:kidn* 'nmlb:m_power node)))
                        (h_power     (and-text (sxml:kidn* 'nmlb:h_power node)))
                        (m_alpha     (and-text (sxml:kidn* 'nmlb:m_alpha node)))
                        (m_beta      (and-text (sxml:kidn* 'nmlb:m_beta node)))
                        (h_alpha     (and-text (sxml:kidn* 'nmlb:h_alpha node)))
                        (h_beta      (and-text (sxml:kidn* 'nmlb:h_beta node)))
                        (m_tau       (and-text (sxml:kidn* 'nmlb:m_tau node)))
                        (m_inf       (and-text (sxml:kidn* 'nmlb:m_inf node)))
                        (h_tau       (and-text (sxml:kidn* 'nmlb:h_tau node)))
                        (h_inf       (and-text (sxml:kidn* 'nmlb:h_inf node)))
                        )
                   
                   `(ncml:hh_ionic_gate (@ (name ,id))
                      ,@(if initial_m `((ncml:initial_m ,initial_m)) `())
                      ,@(if initial_h `((ncml:initial_h ,initial_h)) `())
                      ,@(if m_power `((ncml:m_power ,m_power)) '())
                      ,@(if h_power `((ncml:h_power ,h_power)) '())
                      ,@(if m_alpha `((ncml:m_alpha ,m_alpha)) '())
                      ,@(if h_alpha `((ncml:h_alpha ,h_alpha)) '())
                      ,@(if m_beta  `((ncml:m_beta ,m_beta)) '())
                      ,@(if h_beta  `((ncml:h_beta ,h_beta)) '())
                      ,@(if m_inf   `((ncml:m_inf ,m_inf)) '())
                      ,@(if h_inf   `((ncml:h_inf ,h_inf)) '())
                      ,@(if m_tau   `((ncml:m_tau ,m_tau)) '())
                      ,@(if h_tau   `((ncml:h_tau ,h_tau)) '())
                      )
                   ))
                 )
   
                                
   ))

(define (nineml->model-decls options doc)

  (define (load-ss in)
    (eval `(begin
             ,@sxslt-preamble
             (sxml:make-ss ,@(read in))
             )))

  (define (make-ss-fname dirname fname) 
    (or (and dirname (make-pathname dirname fname)) fname))

  (let* ((source-path   (lookup-def 'source-path options))
         (dirname       (pathname-directory source-path))
         (parse-expr    (or (lookup-def 'parse-expr options) identity))

	 (nmlb:model    ((lambda (x) 
                           (if (null? x) (error 'nineml->model "NineML Biophysics element not found in input document") (car x)))
                         (nmlb:sxpath '(// nmlb:Biophysics) `(*TOP* . ,doc))))
	 (model-name     ($ (or (sxml:attr nmlb:model 'name) (gensym 'model))))

	 (nmlb-ss        (ncml:sxpath '(// nmlb:sxslt) `(*TOP* . ,nmlb:model)))
	 (nmlb-decls     ((lambda (doc) 
			    (if (null? nmlb-ss) doc
				(let ((ss (map 
					   (lambda (x)
                                             (let ((fn (sxml:attr x 'filename)))
                                               (or (and fn (call-with-input-file (make-ss-fname dirname fn) load-ss))
                                                   (call-with-input-string (sxml:text x) load-ss))
                                               ))
					   nmlb-ss)))
				  (fold (lambda (s doc) (stx:apply-templates doc s doc (list))) doc ss))
				))
                          (sxml:kids nmlb:model)))

         (ncml-env       (make-parameter '()))
	 (ncml-decls     (stx:apply-templates (sxml:kids nmlb:model) nineml-ss nmlb-decls ncml-env))
	 (model-decls    (ncml->declarations 
                          (append (delete-duplicates (ncml-env) (lambda (x y) (equal? (car x) (car y)))) ncml-decls)
                          parse-expr))
	 )
    (list model-name model-decls '())))



(define (entry->surface-xml x . rest)
  (let-optionals rest ((ns-prefix "nemo"))

    (let ((ns-prefix (if (or (not ns-prefix) (string-null? ns-prefix)) ""
			 (string-append ns-prefix ":")))
	  (xmlstr (lambda (x) (let recur ((x x)) 
				(if (pair? x) (map recur x) 
				    (let ((v (string->goodHTML (->string x))))
				      (if (pair? v) (string-concatenate v) v)))
				))
		  ))

      (let ((transition-str
	     (lambda (t)
	       (match t
		      (('-> src dst rate) 
		       (sprintf "<~Atransition src=\"~A\" dst=\"~A\">~%<~Arate>~A </~Arate>~% </~Atransition>~%"
				ns-prefix src dst ns-prefix (xmlstr rate) ns-prefix ns-prefix))

		      ((src '-> dst rate) 
		       (sprintf "<~Atransition src=\"~A\" dst=\"~A\">~%<~Arate>~A </~Arate>~% </~Atransition>~%"
				ns-prefix src dst ns-prefix (xmlstr rate) ns-prefix ns-prefix))

		      (('<-> src dst rate1 rate2) 
		       (sprintf "<~Atransition src=\"~A\" dst=\"~A\">~%<~Arate>~A </~Arate>~% </~Atransition>~%<~Atransition src=\"~A\" dst=\"~A\">~%<~Arate>~A </~Arate>~% </~Atransition>~%"
				ns-prefix src dst ns-prefix (xmlstr rate1) ns-prefix ns-prefix
				ns-prefix dst src ns-prefix (xmlstr rate2) ns-prefix ns-prefix
				))

		      ((src '<-> dst rate1 rate2) 
		       (sprintf "<~Atransition src=\"~A\" dst=\"~A\">~%<~Arate>~A </~Arate>~% </~Atransition>~%<~Atransition src=\"~A\" dst=\"~A\">~%<~Arate>~A </~Arate>~% </~Atransition>~%"
				ns-prefix src dst ns-prefix (xmlstr rate1) ns-prefix ns-prefix
				ns-prefix dst src ns-prefix (xmlstr rate2) ns-prefix ns-prefix
				))

		      (else (error 'transition-str "invalid transition " x))))
	       )
	       
	    (ionic-gate-str
	     (lambda (ion #!key 
			  (initial-m-expr #f)
			  (initial-h-expr #f)
			  (m-power #f)
			  (h-power #f)
			  (m-inf-expr #f)
			  (m-tau-expr #f)
			  (h-inf-expr #f)
			  (h-tau-expr #f)
			  (m-alpha-expr #f)
			  (m-beta-expr #f)
			  (h-alpha-expr #f)
			  (h-beta-expr #f))

	       (let ((initial-m-str (or (and initial-m-expr
						  (sprintf "<~Ainitial_m>~A</~Ainitial_m>~%" 
							   ns-prefix (xmlstr initial-m-expr) ns-prefix)) ""))
		     (initial-h-str (or (and initial-h-expr
						  (sprintf "<~Ainitial_h>~A</~Ainitial_h>~%" 
							   ns-prefix (xmlstr initial-h-expr) ns-prefix)) ""))

		     (m-power-str  (or (and m-power
						  (sprintf "<~Am_power>~A</~Am_power>~%" 
							   ns-prefix m-power ns-prefix)) ""))
		     (h-power-str  (or (and h-power
						  (sprintf "<~Ah_power>~A</~Ah_power>~%" 
							   ns-prefix h-power ns-prefix)) ""))

		     (m-inf-str (or (and m-inf-expr
					 (sprintf "<~Am_inf>~A</~Am_inf>~%" 
						  ns-prefix (xmlstr m-inf-expr) ns-prefix)) ""))
		     (m-tau-str (or (and m-tau-expr
					 (sprintf "<~Am_tau>~A</~Am_tau>~%" 
						  ns-prefix (xmlstr m-tau-expr) ns-prefix)) ""))

		     (h-inf-str (or (and h-inf-expr
					 (sprintf "<~Ah_inf>~A</~Ah_inf>~%" 
						  ns-prefix (xmlstr h-inf-expr) ns-prefix)) ""))
		     (h-tau-str (or (and h-tau-expr
					 (sprintf "<~Ah_tau>~A</~Ah_tau>~%" 
						  ns-prefix (xmlstr h-tau-expr) ns-prefix)) ""))

		     (m-alpha-str (or (and m-alpha-expr
					   (sprintf "<~Am_alpha>~A</~Am_alpha>~%" 
						    ns-prefix (xmlstr m-alpha-expr) ns-prefix)) ""))
		     (m-beta-str (or (and m-beta-expr
					  (sprintf "<~Am_beta>~A</~Am_beta>~%" 
						   ns-prefix (xmlstr m-beta-expr) ns-prefix)) ""))

		     (h-alpha-str (or (and h-alpha-expr
					   (sprintf "<~Ah_alpha>~A</~Ah_alpha>~%" 
						    ns-prefix (xmlstr h-alpha-expr) ns-prefix)) ""))
		     (h-beta-str (or (and h-beta-expr
					  (sprintf "<~Ah_beta>~A</~Ah_beta>~%" 
						   ns-prefix (xmlstr h-beta-expr) ns-prefix)) ""))
		     )
		 
		 (sprintf "<~Ahh_ionic_gate name=\"~A\">~A</~Ahh_ionic_gate>~%"
			  ns-prefix ion   
			  (string-append initial-m-str initial-h-str
					 m-power-str h-power-str m-inf-str 
					 m-tau-str h-inf-str h-tau-str
					 m-alpha-str m-beta-str h-alpha-str h-beta-str
					 )
			  ns-prefix))
	       )))

    (match x
	 (('nemo-model name decls)
	  (map entry->surface-xml decls))

	 (('output . names)
	  (string-concatenate (map (lambda (name) (sprintf "<~Aoutput name=\"~A\"/>~%" ns-prefix name)) names)))

	 (('input . names)
	  (string-concatenate (map (lambda (name) 
				     (match name
					    ((and name (? symbol?)) 
					     (sprintf "<~Ainput name=\"~A\"/>~%" ns-prefix name))

					    ((name 'from ns)
					     (sprintf "<~Ainput name=\"~A\" from=\"~A\"/>~%" ns-prefix name ns))
					    
					    ))
				   names)))

	 (('const name '= value)
	  (if (number? value)
	      (sprintf "<~Aconst name=\"~A\" value=\"~A\"/>~%"
		      ns-prefix name value)
	      (sprintf "<~Aconst name=\"~A\">~%~A~%</~Aconst>~%"
		       ns-prefix name (xmlstr value) ns-prefix)
	      ))

	 (((or 'defun 'fun) name args body)
	  (sprintf "<~Adefun name=\"~A\">~%~A~%<~Abody>~A</~Abody>~%</~Adefun>~%"
		   ns-prefix
		   name (string-concatenate (map (lambda (x) (sprintf "<~Aarg>~A</~Aarg>" ns-prefix x ns-prefix)) args)) 
		   ns-prefix (xmlstr body) ns-prefix ns-prefix))
	 
	 ((name '= expr)
	  (sprintf "<~Aasgn name=\"~A\"><~Aexpr>~A</~Aexpr>~%</~Aasgn>~%"
		  ns-prefix name ns-prefix (xmlstr expr) ns-prefix ns-prefix))
	 
	 (('d ( name ) '= expr)
	  (sprintf "<~Arate name=\"~A\"><~Aexpr>~A</~Aexpr>~%</~Arate>~%"
		  ns-prefix name ns-prefix (xmlstr expr) ns-prefix ns-prefix))
	 
	 (('d ( name ) '= expr ('initial initial-expr))
	  (sprintf "<~Arate name=\"~A\"><~Aexpr>~A</~Aexpr>~%<~Ainitial>~A</~Ainitial>~%</~Arate>~%"
		   ns-prefix name ns-prefix (xmlstr expr) ns-prefix ns-prefix (xmlstr initial-expr) ns-prefix ns-prefix))

	 (((or 't 'T 'transient) ( name ) '= expr ('onevent event-expr) ('initial initial-expr))
	  (sprintf "<~Atransient name=\"~A\"><~Aexpr>~A</~Aexpr>~%<~Aonevent>~A</~Aonevent>~%<~Ainitial>~A</~Ainitial>~%</~Atransient>~%"
		   ns-prefix name ns-prefix (xmlstr expr) ns-prefix ns-prefix (xmlstr event-expr) ns-prefix ns-prefix (xmlstr initial-expr) ns-prefix ns-prefix))

	 (('reaction ( name ('transitions . transitions) ('conserve conserve) ('initial . initial-expr) ('open . open) ('power power)))
	  (sprintf "<~Areaction name=\"~A\"><~Aopen>~A</~Aopen>~%<~Apower>~A</~Apower>~%<~Atransitions>~A</~Atransitions>~%<~Ainitial>~A</~Ainitial>~%</~Areaction>~%"
		   ns-prefix name 
		   ns-prefix (string-concatenate (intersperse (map ->string open) ",")) ns-prefix 
		   ns-prefix (xmlstr power) ns-prefix
		   ns-prefix (string-concatenate (map transition-str transitions)) ns-prefix
                   ns-prefix (xmlstr initial-expr) ns-prefix
		   ns-prefix))

	 (('reaction ( name ('transitions . transitions) ('conserve conserve) ('open . open) ('power power)))
	  (sprintf "<~Areaction name=\"~A\"><~Aopen>~A</~Aopen>~%<~Apower>~A</~Apower>~%<~Atransitions>~A</~Atransitions>~%</~Areaction>~%"
		   ns-prefix name 
		   ns-prefix (string-concatenate (intersperse (map ->string open) ",")) ns-prefix 
		   ns-prefix (xmlstr power) ns-prefix
		   ns-prefix (string-concatenate (map transition-str transitions)) ns-prefix
		   ns-prefix))

	 (('reaction ( name ('transitions . transitions) ('open . open) ('power power)))
	  (sprintf "<~Areaction name=\"~A\"><~Aopen>~A</~Aopen>~%<~Apower>~A</~Apower>~%<~Atransitions>~A</~Atransitions>~%</~Areaction>~%"
		   ns-prefix name 
		   ns-prefix (string-concatenate (intersperse (map ->string open) ",")) ns-prefix 
		   ns-prefix (xmlstr power) ns-prefix
		   ns-prefix (string-concatenate (map transition-str transitions)) ns-prefix
		   ns-prefix))

	 
	 (('hh-ionic-gate 
	   (ion
	    ('initial-m  initial-m-expr)
	    ('initial-h  initial-h-expr)
	    ('m-power    m-power)
	    ('h-power    h-power)
	    ('m-inf      m-inf-expr)
	    ('m-tau      m-tau-expr)
	    ('h-inf      h-inf-expr)
	    ('h-tau      h-tau-expr)
	    ))

	  (ionic-gate-str ion 
			  initial-m-expr: initial-m-expr
			  initial-h-expr: initial-h-expr
			  m-power: m-power
			  h-power: h-power
			  m-inf-expr: m-inf-expr
			  m-tau-expr: m-tau-expr
			  h-inf-expr: h-inf-expr
			  h-tau-expr: h-tau-expr))

	 
	 (('hh-ionic-gate 
	   (ion
	    ('initial-m  initial-m-expr)
	    ('m-power    m-power)
	    ('h-power    h-power)
	    ('m-inf      m-inf-expr)
	    ('m-tau      m-tau-expr)
	    ))

	  (ionic-gate-str ion 
			  initial-m-expr: initial-m-expr
			  m-power: m-power
			  h-power: h-power
			  m-inf-expr: m-inf-expr
			  m-tau-expr: m-tau-expr))
	 
	 (('hh-ionic-gate 
	   (ion
	    ('initial-m  initial-m-expr)
	    ('m-power    m-power)
	    ('h-power    h-power)
	    ('m-tau      m-tau-expr)
	    ('m-inf      m-inf-expr)
	    ))

	  (ionic-gate-str ion 
			  initial-m-expr: initial-m-expr
			  m-power: m-power
			  h-power: h-power
			  m-inf-expr: m-inf-expr
			  m-tau-expr: m-tau-expr))
	 
	 (('hh-ionic-gate 
	   (ion
	    ('initial-m  initial-m-expr)
	    ('initial-h  initial-h-expr)
	    ('m-power    m-power)
	    ('h-power    h-power)
	    ('m-alpha      m-alpha-expr)
	    ('m-beta       m-beta-expr)
	    ('h-alpha      h-alpha-expr)
	    ('h-beta       h-beta-expr)
	    ))

	  (ionic-gate-str ion 
			  initial-m-expr: initial-m-expr
			  initial-h-expr: initial-h-expr
			  m-power: m-power
			  h-power: h-power
			  m-alpha-expr: m-alpha-expr
			  m-beta-expr: m-beta-expr
			  h-alpha-expr: h-alpha-expr
			  h-beta-expr: h-beta-expr))
	 
	 (('hh-ionic-gate 
	   (ion
	    ('initial-m  initial-m-expr)
	    ('m-power    m-power)
	    ('h-power    h-power)
	    ('m-alpha      m-alpha-expr)
	    ('m-beta       m-beta-expr)
	    ))

	  (ionic-gate-str ion 
			  initial-m-expr: initial-m-expr
			  m-power: m-power
			  h-power: h-power
			  m-alpha-expr: m-alpha-expr
			  m-beta-expr: m-beta-expr))

	 
	 (('component ('type ty) ('name name) . rest) 
	  (sprintf "<~Acomponent type=\"~A\" name=\"~A\">~%~A</~Acomponent>~%" 
		  ns-prefix ty name (string-concatenate (map entry->surface-xml rest)) ns-prefix ))

	 (('component ('type ty) . rest) 
	  (sprintf "<~Acomponent type=\"~A\">~%~A</~Acomponent>~%" 
		   ns-prefix ty (string-concatenate (map entry->surface-xml rest)) ns-prefix ))

	 (('component ('name name) '= func decls) 
	  (sprintf "<~Acomponent name=\"~A\" functor=\"~A\">~%~A</~Acomponent>~%" 
		   ns-prefix name func (string-concatenate (map entry->surface-xml decls)) ns-prefix ))

	 (('functor ('type ty) ('name name) args '= . rest) 
	  (sprintf "<~Afunctor type=\"~A\" name=\"~A\">~%~A~%~A</~Afunctor>~%" 
		  ns-prefix ty name 
                  (string-concatenate (map (lambda (x) (sprintf "<~Aarg>~A</~Aarg>" ns-prefix x ns-prefix)) args)) 
                  (string-concatenate (map entry->surface-xml rest)) 
                  ns-prefix ))


	 (else (error 'nemo "unknown declaration" x))

	 )))
))


(define (entry->nineml x . rest)
  (let-optionals rest ((ns-prefix-str "Biophysics9ML"))

    (let ((ns-prefix (if (or (not ns-prefix-str) (string-null? ns-prefix-str)) ""
			 (string-append ns-prefix-str ":")))
	  (xmlstr (lambda (x) (let recur ((x x)) 
				(if (pair? x) (map recur x) 
				    (let ((v (string->goodHTML (->string x))))
				      (if (pair? v) (string-concatenate v) v)))
				))
		  ))

      (let ((unit-str
             (lambda (u)
               (or (and u (sprintf "<~Aunit>~A</~Aunit>" ns-prefix u ns-prefix)) "")))

            (conserve-str
             (lambda (e)
               (match e
                      ((v '= rhs)
                       (sprintf "<~Aconserve val=\"~A\">~%~A </~Aconserve>~%"
				ns-prefix v (xmlstr rhs) ns-prefix))
		      (else (error 'conserve-str "invalid conservation equation " e))))
	       )

            (transition-str
	     (lambda (t)
	       (match t
		      (('-> src dst rate) 
		       (sprintf "<~Atransition src=\"~A\" dst=\"~A\">~%<~Arate>~A </~Arate>~% </~Atransition>~%"
				ns-prefix src dst ns-prefix (xmlstr rate) ns-prefix ns-prefix))

		      ((src '-> dst rate) 
		       (sprintf "<~Atransition src=\"~A\" dst=\"~A\">~%<~Arate>~A </~Arate>~% </~Atransition>~%"
				ns-prefix src dst ns-prefix (xmlstr rate) ns-prefix ns-prefix))

		      (('<-> src dst rate1 rate2) 
		       (sprintf "<~Atransition src=\"~A\" dst=\"~A\">~%<~Arate>~A </~Arate>~% </~Atransition>~%<~Atransition src=\"~A\" dst=\"~A\">~%<~Arate>~A </~Arate>~% </~Atransition>~%"
				ns-prefix src dst ns-prefix (xmlstr rate1) ns-prefix ns-prefix
				ns-prefix dst src ns-prefix (xmlstr rate2) ns-prefix ns-prefix
				))

		      ((src '<-> dst rate1 rate2) 
		       (sprintf "<~Atransition src=\"~A\" dst=\"~A\">~%<~Arate>~A </~Arate>~% </~Atransition>~%<~Atransition src=\"~A\" dst=\"~A\">~%<~Arate>~A </~Arate>~% </~Atransition>~%"
				ns-prefix src dst ns-prefix (xmlstr rate1) ns-prefix ns-prefix
				ns-prefix dst src ns-prefix (xmlstr rate2) ns-prefix ns-prefix
				))

		      (else (error 'transition-str "invalid transition " x))))
	       )
	       
	    (ionic-gate-str
	     (lambda (ion #!key 
			  (initial-m-expr #f)
			  (initial-h-expr #f)
			  (m-power #f)
			  (h-power #f)
			  (m-inf-expr #f)
			  (m-tau-expr #f)
			  (h-inf-expr #f)
			  (h-tau-expr #f)
			  (m-alpha-expr #f)
			  (m-beta-expr #f)
			  (h-alpha-expr #f)
			  (h-beta-expr #f))

	       (let ((initial-m-str (or (and initial-m-expr
						  (sprintf "<~Ainitial_m>~A</~Ainitial_m>~%" 
							   ns-prefix (xmlstr initial-m-expr) ns-prefix)) ""))
		     (initial-h-str (or (and initial-h-expr
						  (sprintf "<~Ainitial_h>~A</~Ainitial_h>~%" 
							   ns-prefix (xmlstr initial-h-expr) ns-prefix)) ""))

		     (m-power-str  (or (and m-power
						  (sprintf "<~Am_power>~A</~Am_power>~%" 
							   ns-prefix m-power ns-prefix)) ""))
		     (h-power-str  (or (and h-power
						  (sprintf "<~Ah_power>~A</~Ah_power>~%" 
							   ns-prefix h-power ns-prefix)) ""))

		     (m-inf-str (or (and m-inf-expr
					 (sprintf "<~Am_inf>~A</~Am_inf>~%" 
						  ns-prefix (xmlstr m-inf-expr) ns-prefix)) ""))
		     (m-tau-str (or (and m-tau-expr
					 (sprintf "<~Am_tau>~A</~Am_tau>~%" 
						  ns-prefix (xmlstr m-tau-expr) ns-prefix)) ""))

		     (h-inf-str (or (and h-inf-expr
					 (sprintf "<~Ah_inf>~A</~Ah_inf>~%" 
						  ns-prefix (xmlstr h-inf-expr) ns-prefix)) ""))
		     (h-tau-str (or (and h-tau-expr
					 (sprintf "<~Ah_tau>~A</~Ah_tau>~%" 
						  ns-prefix (xmlstr h-tau-expr) ns-prefix)) ""))

		     (m-alpha-str (or (and m-alpha-expr
					   (sprintf "<~Am_alpha>~A</~Am_alpha>~%" 
						    ns-prefix (xmlstr m-alpha-expr) ns-prefix)) ""))
		     (m-beta-str (or (and m-beta-expr
					  (sprintf "<~Am_beta>~A</~Am_beta>~%" 
						   ns-prefix (xmlstr m-beta-expr) ns-prefix)) ""))

		     (h-alpha-str (or (and h-alpha-expr
					   (sprintf "<~Ah_alpha>~A</~Ah_alpha>~%" 
						    ns-prefix (xmlstr h-alpha-expr) ns-prefix)) ""))
		     (h-beta-str (or (and h-beta-expr
					  (sprintf "<~Ah_beta>~A</~Ah_beta>~%" 
						   ns-prefix (xmlstr h-beta-expr) ns-prefix)) ""))
		     )
		 
		 (sprintf "<~Ahh_ionic_gate name=\"~A\">~A</~Ahh_ionic_gate>~%"
			  ns-prefix ion   
			  (string-append initial-m-str initial-h-str
					 m-power-str h-power-str m-inf-str 
					 m-tau-str h-inf-str h-tau-str
					 m-alpha-str m-beta-str h-alpha-str h-beta-str
					 )
			  ns-prefix))
	       )))

    (match x
	 (('nemo-model name decls)
          `(,(sprintf "<NineML xmlns=\"http://nineml.org/9ML/1.0\">~%" )
            ,(sprintf "<~ABiophysics xmlns:~A=\"http://www.nineml.org/Biophysics\" name=\"~A\">~%" ns-prefix ns-prefix-str name)
            ,@(map entry->nineml decls) 
            ,(sprintf "</~ABiophysics>~%" ns-prefix )
            ,(sprintf "</NineML>~%")))

	 (('output . names)
	  (string-concatenate (map (lambda (name) (sprintf "<~AAnalogPort mode='send' name=\"~A\"/>~%" ns-prefix name)) names)))

	 (('input . names)
	  (string-concatenate (map (lambda (name) 
				     (match name
					    ((and name (? symbol?)) 
					     (sprintf "<~AAnalogPort mode='receive' name=\"~A\"/>~%" ns-prefix name))

					    (((and name (? symbol?)) ('unit u))
					     (sprintf "<~AAnalogPort mode='receive' name=\"~A\" unit=\"~A\"/>~%" ns-prefix name u))

					    ((name 'from ns)
					     (sprintf "<~AAnalogPort mode='receive' name=\"~A\" from=\"~A\"/>~%" ns-prefix name ns))

					    ((name 'from ns ('unit u))
					     (sprintf "<~AAnalogPort mode='receive' name=\"~A\" from=\"~A\" unit=\"~A\"/>~%" ns-prefix name ns u))

					    (else (error 'entry->nineml "invalid input declaration" x))
					    ))
				   names)))

	 (('const name '= value . rest)
          (let* ((u     (lookup-def 'unit rest)))
            (if (number? value)
                (sprintf "<~AParameter name=\"~A\">~%~A<~Avalue>~A</~Avalue>~%</~AParameter>~%"
                         ns-prefix name (unit-str u) ns-prefix value ns-prefix ns-prefix)
                (sprintf "<~AParameter name=\"~A\">~%~A<~Avalue>~A</~Avalue>~%</~AParameter>~%"
                         ns-prefix name (unit-str u) ns-prefix (xmlstr value) ns-prefix ns-prefix)
                )))

	 (((or 'defun 'fun) name args body)
	  (sprintf "<~AAlias name=\"~A\" arguments=\"~A\">~%<~Abody>~A</~Abody>~%</~AAlias>~%"
		   ns-prefix
		   name (string-concatenate (intersperse (map ->string args) ","))
		   ns-prefix (xmlstr body) ns-prefix ns-prefix))
	 
	 ((name '= expr . rest)
          (let ((u (lookup-def 'unit rest)))
            (sprintf "<~AAssignment name=\"~A\">~A<~Arhs>~A</~Arhs>~%</~AAssignment>~%"
                     ns-prefix name (unit-str u) ns-prefix (xmlstr expr) ns-prefix ns-prefix)))
	 
	 (('d ( name ) '= expr . rest)
          (let ((u (lookup-def 'unit rest))
                (initial-expr (lookup-def 'initial rest)))
            (if initial-expr
                (sprintf "<~ATimeDerivative variable=\"~A\">~A<~Arhs>~A</~Arhs>~%<~Ainitial>~A</~Ainitial>~%</~ATimeDerivative>~%"
                         ns-prefix name (unit-str u) ns-prefix (xmlstr expr) ns-prefix ns-prefix (xmlstr initial-expr) ns-prefix ns-prefix)
                (sprintf "<~ATimeDerivative variable=\"~A\">~A<~Arhs>~A</~Arhs>~%</~ATimeDerivative>~%"
                         ns-prefix name (unit-str u) ns-prefix (xmlstr expr) ns-prefix ns-prefix)
                )))

	 (((or 't 'T 'transient) ( name ) '= expr . rest)
          (let ((u (lookup-def 'unit rest))
                (event-expr (lookup-def 'onevent rest))
                (initial-expr (lookup-def 'initial rest)))

            (if (not event-expr) (error 'entry->nineml "invalid transient declaration" x))

            (sprintf "<~ATransient variable=\"~A\">~A<~Arhs>~A</~Arhs>~%<~Aonevent>~A</~Aonevent>~%<~Ainitial>~A</~Ainitial>~%</~ATransient>~%"
                     ns-prefix name  (unit-str u) ns-prefix (xmlstr expr) ns-prefix ns-prefix (xmlstr event-expr) ns-prefix ns-prefix (xmlstr initial-expr) ns-prefix ns-prefix)))

	 (('reaction ( name . rest))
          (let ((u            (lookup-def 'unit rest))
                (transitions  (lookup-def 'transitions rest))
                (conserve     (lookup-def 'conserve rest))
                (initial-expr (lookup-def 'initial rest))
                (open  (let ((v (lookup-def 'open rest)))
                         (if (symbol? v) (list v) v)))
                (power (lookup-def 'power rest)))

            (if (not (and transitions open)) (error 'entry->nineml "invalid reaction declaration" x))
            
            (cond ((and conserve initial-expr power)
                   (sprintf "<~AReaction variable=\"~A\">~A<~Aopen>~A</~Aopen>~%<~Apower>~A</~Apower>~%~A<~Atransitions>~A</~Atransitions>~%<~Ainitial>~A</~Ainitial>~%</~AReaction>~%"
                            ns-prefix name (unit-str u)  
                            ns-prefix (string-concatenate (intersperse (map ->string open) ",")) ns-prefix 
                            ns-prefix (xmlstr power) ns-prefix
                            (conserve-str (car conserve)) 
                            ns-prefix (string-concatenate (map transition-str transitions)) ns-prefix
                            ns-prefix (xmlstr initial-expr) ns-prefix
                            ns-prefix))

                  ((and conserve power)
                   (sprintf "<~AReaction variable=\"~A\">~A<~Aopen>~A</~Aopen>~%<~Apower>~A</~Apower>~%~A<~Atransitions>~A</~Atransitions>~%</~AReaction>~%"
                            ns-prefix name (unit-str u)
                            ns-prefix (string-concatenate (intersperse (map ->string open) ",")) ns-prefix 
                            ns-prefix (xmlstr power) ns-prefix
                            (conserve-str (car conserve)) 
                            ns-prefix (string-concatenate (map transition-str transitions)) ns-prefix
                            ns-prefix))

                  (power
                   (sprintf "<~AReaction variable=\"~A\">~A<~Aopen>~A</~Aopen>~%<~Apower>~A</~Apower>~%<~Atransitions>~A</~Atransitions>~%</~AReaction>~%"
                            ns-prefix name (unit-str u)
                            ns-prefix (string-concatenate (intersperse (map ->string open) ",")) ns-prefix 
                            ns-prefix (xmlstr power) ns-prefix
                            ns-prefix (string-concatenate (map transition-str transitions)) ns-prefix
                            ns-prefix))

                  (else
                   (sprintf "<~AReaction variable=\"~A\">~A<~Aopen>~A</~Aopen>~%<~Atransitions>~A</~Atransitions>~%</~AReaction>~%"
                            ns-prefix name (unit-str u)
                            ns-prefix (string-concatenate (intersperse (map ->string open) ",")) ns-prefix 
                            ns-prefix (string-concatenate (map transition-str transitions)) ns-prefix
                            ns-prefix))
                  )))

	 
	 (('hh-ionic-gate 
	   (ion
	    ('initial-m  initial-m-expr)
	    ('initial-h  initial-h-expr)
	    ('m-power    m-power)
	    ('h-power    h-power)
	    ('m-inf      m-inf-expr)
	    ('m-tau      m-tau-expr)
	    ('h-inf      h-inf-expr)
	    ('h-tau      h-tau-expr)
	    ))

	  (ionic-gate-str ion 
			  initial-m-expr: initial-m-expr
			  initial-h-expr: initial-h-expr
			  m-power: m-power
			  h-power: h-power
			  m-inf-expr: m-inf-expr
			  m-tau-expr: m-tau-expr
			  h-inf-expr: h-inf-expr
			  h-tau-expr: h-tau-expr))

	 
	 (('hh-ionic-gate 
	   (ion
	    ('initial-m  initial-m-expr)
	    ('m-power    m-power)
	    ('h-power    h-power)
	    ('m-inf      m-inf-expr)
	    ('m-tau      m-tau-expr)
	    ))

	  (ionic-gate-str ion 
			  initial-m-expr: initial-m-expr
			  m-power: m-power
			  h-power: h-power
			  m-inf-expr: m-inf-expr
			  m-tau-expr: m-tau-expr))
	 
	 (('hh-ionic-gate 
	   (ion
	    ('initial-m  initial-m-expr)
	    ('m-power    m-power)
	    ('h-power    h-power)
	    ('m-tau      m-tau-expr)
	    ('m-inf      m-inf-expr)
	    ))

	  (ionic-gate-str ion 
			  initial-m-expr: initial-m-expr
			  m-power: m-power
			  h-power: h-power
			  m-inf-expr: m-inf-expr
			  m-tau-expr: m-tau-expr))
	 
	 ((or 
           ('hh-ionic-gate 
            (ion
             ('initial-m  initial-m-expr)
             ('initial-h  initial-h-expr)
             ('m-power    m-power)
             ('h-power    h-power)
             ('m-alpha      m-alpha-expr)
             ('m-beta       m-beta-expr)
             ('h-alpha      h-alpha-expr)
             ('h-beta       h-beta-expr)
             ))
           ('hh-ionic-gate 
            (ion
             ('initial-m  initial-m-expr)
             ('initial-h  initial-h-expr)
             ('m-power    m-power)
             ('h-power    h-power)
             ('m-alpha      m-alpha-expr)
             ('h-alpha      h-alpha-expr)
             ('m-beta       m-beta-expr)
             ('h-beta       h-beta-expr)
             ))
           )

	  (ionic-gate-str ion 
			  initial-m-expr: initial-m-expr
			  initial-h-expr: initial-h-expr
			  m-power: m-power
			  h-power: h-power
			  m-alpha-expr: m-alpha-expr
			  m-beta-expr: m-beta-expr
			  h-alpha-expr: h-alpha-expr
			  h-beta-expr: h-beta-expr))
	 
	 (('hh-ionic-gate 
	   (ion
	    ('initial-m  initial-m-expr)
	    ('m-power    m-power)
	    ('h-power    h-power)
	    ('m-alpha      m-alpha-expr)
	    ('m-beta       m-beta-expr)
	    ))

	  (ionic-gate-str ion 
			  initial-m-expr: initial-m-expr
			  m-power: m-power
			  h-power: h-power
			  m-alpha-expr: m-alpha-expr
			  m-beta-expr: m-beta-expr))

	 
	 (('component ('type ty) ('name name) . rest) 
	  (sprintf "<~AComponent type=\"~A\" name=\"~A\">~%~A</~AComponent>~%~%" 
		  ns-prefix ty name (string-concatenate (map entry->nineml rest)) ns-prefix ))

	 (('component ('type ty) . rest) 
	  (sprintf "<~AComponent type=\"~A\">~%~A</~AComponent>~%~%" 
		   ns-prefix ty (string-concatenate (map entry->nineml rest)) ns-prefix ))

	 (('component ('name name) '= func decls) 
	  (sprintf "<~AComponent name=\"~A\" definition=\"~A\">~%<~Aproperties>~A</~Aproperties></~AComponent>~%~%" 
		   ns-prefix name func ns-prefix (string-concatenate (map entry->nineml decls)) ns-prefix ns-prefix ))

	 ((or ('functor ('type ty) ('name name) args '= . rest) 
              ('functor ('type ty) ('name name) args . rest) )
	  (sprintf "<~AComponentClass type=\"~A\" name=\"~A\">~%<~AInterface>~A</~AInterface>~%~A</~AComponentClass>~%~%" 
		  ns-prefix ty name 
                  ns-prefix (string-concatenate (map (lambda (x) (sprintf "<~AParameter name=\"~A\"/>~%" ns-prefix x)) args)) ns-prefix
                  (string-concatenate (map entry->nineml rest)) 
                  ns-prefix ))


	 (else (error 'entry->nineml "unknown declaration" x))

	 )))
))


(define (partition-model opt decls)

  (define (update-bkts name decl bkts)
    (let ((bkt (alist-ref name bkts)))
      (if bkt
          (alist-update name (cons decl bkt) bkts)
          (alist-update name (list decl) bkts)
          )))

  (let recur ((bkts '()) (toplevel '()) (decls decls))
    (if (null? decls)
	(list bkts (reverse toplevel))
	(let ((decl (car decls)))
	  (if (opt 'debug)
	      (begin (print "partition-model: decl = ")
		     (pp decl)))
	  (match decl

                 (((or 'component 'COMPONENT)
                   ((or 'type 'TYPE) typ) 
                   ((or 'name 'NAME) name) . rest)
                  (recur (update-bkts name decl bkts)
                         toplevel (cdr decls)))
                 
                 (((or 'component 'COMPONENT)
                   ((or 'name 'NAME) name)  . rest)
                  (recur (update-bkts name decl bkts)
                         toplevel (cdr decls)))
                 
		 (else (recur bkts (cons decl toplevel) (cdr decls)))))
	))
  )


(define (process-model opt source-path in-format prefix sys model-decls iexpr? parse-expr)
		
  (match-let ((($ nemo:quantity 'DISPATCH  dis) 
	       (hash-table-ref sys (nemo-intern 'dispatch))))
				    
     (let* (
	    (sysname             ((lambda (x) (or (and prefix ($ (s+ prefix "_" x))) x)) ((dis 'sysname) sys)))
	    (dirname             (pathname-directory source-path))
	    (plain-fname         (make-output-fname dirname sysname ".txt"  (opt 'plain) ))
	    (sxml-fname          (make-output-fname dirname sysname  ".sxml" (opt 'sxml) ))
	    (surface-xml-fname   (make-output-fname dirname sysname ".xml"  (opt 'surface-xml) ))
	    (nineml-fname        (make-output-fname dirname sysname ".9ml"  (opt 'surface-nineml) ))
	    (xml-fname           (make-output-fname dirname sysname ".xml"  (opt 'xml) ))
	    (pyparams-fname      (make-output-fname dirname sysname ".py"  (opt 'pyparams) ))
	    (mod-fname           (make-output-fname dirname sysname ".mod"  (opt 'nmodl)))
	    (vclamp-ses-fname    (make-output-fname dirname sysname "_vclamp.hoc" (opt 'vclamp-hoc) ))
	    (vclamp-octave-fname (make-output-fname dirname sysname "_vclamp.m" (opt 'vclamp-octave) ))
	    (iclamp-ses-fname    (make-output-fname dirname sysname "_iclamp.hoc" (opt 'iclamp-hoc) ))
	    (iclamp-sli-fname    (make-output-fname dirname sysname "_iclamp.sli" (opt 'iclamp-nest) ))
		  
	    (pyparams       (opt 'pyparams))
	    (nest           (and nemo-nest? (opt 'nest)))
	    (matlab         (opt 'matlab))
	    (octave         (opt 'octave))
	    (vclamp-hoc     (opt 'vclamp-hoc))
	    (vclamp-octave  (opt 'vclamp-octave))
	    (iclamp-hoc     (opt 'iclamp-hoc))
	    (iclamp-nest    (opt 'iclamp-nest))

	    (nmodl-method
	     (let ((method  (or ($ (opt 'nmodl-method) ) (defopt 'nmodl-method))))
	       (case method
		 ((adams runge euler adeuler heun adrunge gear
			 newton simplex simeq seidel sparse derivimplicit cnexp clsoda
			 after_cvode cvode_t cvode_t_v expeuler #f) method)
		 (else (error "unknown NMODL method " method)))))
		   
	    (octave-method
	     (let ((method  ($ (opt 'octave-method) )))
	       (case method
		 ((cvode lsode odepkg ode2r ode5r odesx oders) method)
		 ((#f) 'lsode)
		 (else (error "unknown Octave method " method)))))
				   
	    (nest-method
	     (and nemo-nest?
		  (let ((method  ($ (opt 'nest-method) )))
		    (case method
		      ((cvode ida gsl #f) method)
		      (else (error "unknown NEST method " method))))))
			
	    (nest-ss-method
	     (and nemo-nest?
		  (let ((method  ($ (opt 'nest-ss-method) )))
		    (case method
		      ((gsl kinsol #f) method)
		      (else (error "unknown NEST steady state solving method " method))))))
			
            (nest-abstol
             (and nemo-nest?
                  (opt 'nest-abstol)))
	   
            (nest-reltol
             (and nemo-nest?
                  (opt 'nest-reltol)))
	   
            (nest-maxstep
             (and nemo-nest?
                  (opt 'nest-maxstep)))
	   
	    (parse-expr  (case in-format
			   ((sxml xml nineml)    identity)
			   ((sexp)        identity)
			   ((ixml)        (lambda (x #!optional loc) 
					    (let ((xs (if (string? x) x
							  (string-concatenate
							   (map (lambda (el)
								  (if (string? el) el
								      (if (equal? el '(divide)) " / "
									  (->string el))))
								x)))))
					      (nemo:parse-string-expr xs loc))))
			   ((nemo)        (if iexpr? 
					      (lambda (x #!optional loc) 
						(if (string? x) (nemo:parse-string-expr x loc)
						    (nemo:parse-sym-expr x loc)))
					      nemo:parse-sym-expr))
			   (else    (error 'nemo "unknown input format" in-format))))
	    
	    )
       
       (if (or (and xml-fname surface-xml-fname) 
               (and xml-fname nineml-fname)
               (and nineml-fname surface-xml-fname))
	   (error 'nemo "only one of --xml, --surface-xml, and --nineml options are permitted"))
       
       (if plain-fname
	   (with-output-to-file plain-fname 
	     (lambda () (pretty-print (model->text sys parse-expr)))))
       
       (if sxml-fname
	   (with-output-to-file sxml-fname 
	     (lambda () (pretty-print (model->ncml sys parse-expr)))))
       
       (if xml-fname
	   (let* ((doc  (model->ncml sys parse-expr))
		  (doc1 (ensure-xmlns
			 (cond ((eq? (car doc) '*TOP*) (assoc 'ncml:model (cdr doc)))
			       (else doc)))))
	     (with-output-to-file xml-fname 
	       (lambda () (print-fragments (generate-XML `(begin ,doc1)))))))
       
       (if surface-xml-fname   
	   (with-output-to-file surface-xml-fname 
	     (lambda () (print-fragments (map entry->surface-xml model-decls)))))
       
       (if nineml-fname   
	   (with-output-to-file nineml-fname 
	     (lambda () (print-fragments (entry->nineml `(nemo-model ,sysname ,model-decls))))))
       
       (if mod-fname
	   (with-output-to-file 
	       mod-fname  (lambda () 
			    (model->nmodl 
                             `(
                               (method  . ,nmodl-method)
                               (kinetic . ,(opt 'nmodl-kinetic))
                               (dump-template-env . ,(opt 'dump-template-env))
                               )
                             sys))))
       
       (if octave (model->octave `((filename  . ,(or (and (string? octave) (pathname-file octave)) octave))
				   (dirname   . ,(or (and (string? octave) (pathname-directory octave)) dirname))
                                   )
				 sys))
       
       (if matlab (model->matlab `((dirname . ,(or (and (string? matlab) matlab) dirname))) sys))

       (if pyparams
	   (model->pyparams `((filename . ,pyparams-fname)
                              (mode . ,(if (opt 'partition) 'single 'multiple)))
                            sys))

       
       (if (and nemo-nest? nest)
	   (model->nest `((dirname . ,(or (and (string? nest) nest) dirname))
			  (method    . ,nest-method)
			  (ss-method . ,nest-ss-method)
			  (abstol    . ,nest-abstol)
			  (reltol    . ,nest-reltol)
			  (maxstep   . ,nest-maxstep)
                          (dump-template-env . ,(opt 'dump-template-env))
                          )
			sys))
       
       (if vclamp-hoc (model->vclamp-hoc `((filename . ,vclamp-ses-fname)
                                           
					   )
					 sys))
       
       (if vclamp-octave (model->vclamp-octave `((filename . ,vclamp-octave-fname)
                                                 (octave-method . ,(case octave-method
                                                                     ((odepkg) 'ode2r)
                                                                     (else octave-method)))
						 )
					       sys))

       (if iclamp-hoc (model->iclamp-hoc `((filename . ,iclamp-ses-fname)
					   )
					 sys))

       (if iclamp-nest (model->iclamp-nest `((filename . ,iclamp-sli-fname)
					     )
					   sys))
       ))
  )



(define (process-template model-name template-name template-args template-out user-templates source-path)

  (let (
	(template-vars (cons (cons 'model_name
				   (ersatz:Tstr (->string model-name)) )
			     (map (lambda (x) 
				    (let ((kv (string-split x "=")))
				      (cons ($ (car kv))
					    (ersatz:Tstr (cadr kv)))))
				  template-args)))
	)

    (let* ((dirname (pathname-directory source-path))
	   (output-name (if (string-prefix? "." template-out)
			    (make-pathname dirname (s+ model-name template-out)) 
			    (make-pathname dirname (s+ model-name "_" template-out)) )))
      (with-output-to-file output-name
	(lambda () (instantiate-template* user-templates template-name template-vars))
	))
    ))



(define (detect-xml-type doc)
  (let* (
	 (ncml:model    ((lambda (x) 
			   (and (not (null? x)) (car x)))
			 (ncml:sxpath '(// ncml:model) `(*TOP* . ,doc))))
         (nineml:biophys ((lambda (x) 
                            (and (not (null? x)) (car x)))
			 (ncml:sxpath '(// nmlb:Biophysics) `(*TOP* . ,doc))))
	 (membraneprops  (ncml:sxpath '(// cell biophysicalProperties membraneProperties) 
				      `(*TOP* . ,ncml:model)))
	 )
    (cond (nineml:biophys 'nineml)
          (membraneprops 'ixml)
	  (else 'xml))
    ))


(define (model-source->model source-path in-format model-name model-decls user-templates iexpr parse-expr)

  (case in-format
    ((sxml xml ixml nineml)
     (SingleModel source-path in-format model-name
                  (ncml-model-decls->model
                   `((hh-markov . ,(opt 'hh-markov))
                     (parse-expr . ,parse-expr)) 
                   model-name model-decls)
                  model-decls user-templates iexpr parse-expr))
    
    ((sexp nemo)
     (SingleModel source-path in-format model-name
                  (sexp-model-decls->model 
                   `((hh-markov . ,(opt 'hh-markov)))
                   model-name model-decls parse-expr)
                  model-decls user-templates iexpr parse-expr))
    
    (else (error 'nemo "invalid input format"))
    ))


(define (model-source->model-parts opt source-path in-format 
                                   model-name model-decls 
                                   user-templates iexpr parse-expr)
  (let ((pmodels (partition-model opt model-decls)))
    (if (opt 'debug)
        (begin (print "length pmodels = " (length pmodels))
               (print "pmodels = " )
               (pp pmodels)))
    (let ((model-parts
           (match-let (((bkts toplevel) pmodels))
                      (map (lambda (bkt)
                             (let ((part-decls (append toplevel (cdr bkt)))
                                   (part-name (car bkt)))
                               
                               (case in-format
                                 ((sxml xml ixml nineml)
                                  (ModelPart source-path in-format model-name part-name
                                             (ncml-model-decls->model
                                              `((hh-markov . ,(opt 'hh-markov))
                                                (parse-expr . ,parse-expr)) 
                                              ($ (s+ model-name "_" (car bkt))) part-decls)
                                             part-decls model-decls user-templates iexpr parse-expr)
                                  )
                                 
                                 ((sexp nemo)
                                  (ModelPart source-path in-format model-name part-name
                                             (sexp-model-decls->model
                                              `((hh-markov . ,(opt 'hh-markov)))
                                              ($ (s+ model-name "_" (car bkt))) part-decls parse-expr)
                                             part-decls model-decls user-templates iexpr parse-expr)
                                  )
                                 
                                 (else (error 'nemo "invalid input format" in-format))
                                 )))
                           bkts))
           ))
      model-parts
      )))


  
(define (main opt operands)

  (if (opt 'version)
      (begin
	(print (nemo:version-string))
	(exit 0)))

  (let ((v (opt 'default-units)))
    (if v
        (nemo:default-units (fold (lambda (x ax) (alist-update (car x) (cdr x) ax))
                                  (nemo:default-units) v))
        ))

  (if (opt 'print-default-units)
      (begin
        (for-each (lambda (x)
                    (printf "~A: ~A~%" (nemo:quantity-name (car x)) (cdr x)))
                  (nemo:default-units))))

  (if (opt 'debug)
      (nemo:fetch-verbose #t))
      
  (if (null? operands)

      (nemo:usage)

      (let* (
	    (model-sources
	     (map (lambda (operand)
		    (let* ((read-xml   (lambda (name) 
                                         (call-with-input-file name
                                           (lambda (port) 
                                             (ssax:xml->sxml port
                                                             '((ncml . "ncml")
                                                               (nmlb . "http://www.nineml.org/Biophysics")))
                                             ))
                                         ))
			   (read-sexp  (lambda (name) (call-with-input-file name read)))
			   (read-iexpr (lambda (name) (call-with-input-file name 
							(lambda (port) 
							  (let ((content
								 (iexpr:tree->list
								  (iexpr:parse operand port))))
							    (car content))))))
			   
			   (in-format  (cond ((opt 'input-format) => 
					      (lambda (x) 
						(case ($ x)
						  ((nemo)        'nemo)
						  ((s-exp sexp)  'sexp)
						  ((xml)         'xml)
						  ((ixml)        'ixml)
						  ((sxml)        'sxml)
						  ((9ml 9ML nineml) 'nineml)
						  (else          (error 'nemo "unknown input format" x)))))
					     (else  (case ((lambda (x) (or (not x) ($ x)))
							   (pathname-extension operand))
						      ((s-exp sexp)  'sexp)
						      ((sxml)  'sxml)
						      ((xml 9ml 9ML nineml)   (detect-xml-type (read-xml operand)))
						      (else    'nemo)))))

			   (doc.iexpr   (case in-format
					 ((nemo)  
					  (let ((content (read-sexp operand)))
					    (if (eq? content 'nemo-model)
						(cons (read-iexpr operand) #t)
						(cons content #f))))
					 ((sxml sexp)  
					  (cons (read-sexp operand) #f))
					 ((xml ixml nineml)
					  (cons (read-xml operand) #f))
					 (else    (error 'nemo "unknown input format" in-format))))

			   (dd          (if (opt 'debug)
					    (pp (car doc.iexpr))))
			   
			   (parse-expr  (case in-format
					  ((sxml sexp)         identity)
					  ((nemo)              (if (cdr doc.iexpr) 
								   (lambda (x #!optional loc) 
								     (if (string? x) (nemo:parse-string-expr x loc)
									 (nemo:parse-sym-expr x loc)))
								   nemo:parse-sym-expr))
					  ((xml)               (lambda (x #!optional loc) 
								 (ncml-expr->expr x)))
					  ((ixml nineml)       (lambda (x #!optional loc) 
								 (nemo:parse-string-expr x loc)))
					  (else    (error 'nemo "unknown input format" in-format))))
			   
			   (model-name.model-decls
			    (case in-format
                              ((nineml)            (nineml->model-decls 
						    `((parse-expr . ,parse-expr)
						      (debug . ,(opt 'debug) )
                                                      (source-path . ,operand)
                                                      )
						    (car doc.iexpr)))
			      ((sxml xml ixml)     (ncml->model-decls 
						    `((parse-expr . ,parse-expr)
						      (debug . ,(opt 'debug) )
                                                      (source-path . ,operand)
                                                      )
						    (car doc.iexpr)))
			      ((sexp nemo)         (sexp->model-decls (car doc.iexpr)))
			      (else    (error 'nemo "unknown input format" in-format))))

			   )

		       (ModelSource  operand in-format
				     (car model-name.model-decls)
				     (filter (lambda (x) (not (null? x))) (cadr model-name.model-decls))
				     (match model-name.model-decls 
					    ((_ _ user-templates)
					     user-templates)
					    (else '()))
				     (cdr doc.iexpr) 
				     parse-expr)
		      ))
		  operands))

	    (models
	       (if (opt 'partition)

		    (let recur ((srcs model-sources) (ax '()))
		      (if (null? srcs) ax
			  (let ((src (car srcs)))
			    (cases nemo:model src

				   (ModelSource (source-path in-format model-name model-decls user-templates iexpr parse-expr)
                                                (recur (cdr srcs)
                                                       (append (model-source->model-parts opt source-path in-format 
                                                                                          model-name model-decls 
                                                                                          user-templates iexpr parse-expr) ax)))

                                   (else (error 'nemo "invalid model source" src)))
			    )))
				 
		      (map (lambda (x) 
			     (cases nemo:model x

				    (ModelSource (source-path in-format model-name model-decls user-templates iexpr parse-expr)
                                                 (model-source->model source-path in-format model-name 
                                                                      model-decls user-templates iexpr parse-expr))


				    (else (error 'name "invalid model source" x))))
			   
			   model-sources))
	       )
	    )

	
	(let ((template-insts (opt 'template)))

          (for-each
           
           (lambda (model)
             
             (cases nemo:model model
                    
                    (SingleModel (source-path in-format model-name sys model-decls user-templates iexpr? parse-expr)
				 
                                 (process-model opt source-path in-format #f sys model-decls iexpr? parse-expr)
                                 
                                 (if template-insts
                                     (for-each
                                      (lambda (template-inst)
                                        (match-let (((template-name . template-args)
                                                     (string-split template-inst ":")))
                                                   (let ((output-file-suffix (or (opt 'template-prefix) template-name)))
                                                     (process-template model-name template-name template-args 
                                                                       output-file-suffix user-templates source-path))
                                                   ))
                                      template-insts))
                                 )

		  
		    (ModelPart (source-path in-format model-name part-name sys model-decls parent-decls user-templates iexpr? parse-expr)

			       (process-model opt source-path in-format #f sys model-decls iexpr? parse-expr)
			       
			       (if template-insts
                                   (for-each
                                    (lambda (template-inst)
                                      (match-let (((template-name . template-args)
                                                   (string-split template-inst ":")))
                                                 (let ((output-file-suffix (or (opt 'template-prefix) template-name)))
                                                   (process-template (s+ model-name "_" part-name)
                                                                     template-name template-args 
                                                                     output-file-suffix user-templates source-path))
                                                 ))
                                    template-insts))
                               )
		  
		  (else (error 'nemo "invalid model" model))))

           models))
        )
      ))


(main opt (opt '@))

