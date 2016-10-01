;;       
;; 
;; An extension for generating voltage clamp scripts from NEMO models.
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

(module nemo-vclamp

	(nemo:vclamp-translator)

	(import scheme chicken utils extras data-structures lolevel srfi-1 srfi-13 srfi-69)

	(require-extension lolevel datatype matchable strictly-pretty 
			   varsubst datatype 
			   nemo-core nemo-utils nemo-gate-complex nemo-defaults
                           nemo-matlab)

(define (default-conc? x)
  (match x ((label in out) #t) (else #f))
  )

(define-datatype vclamp-spec vclamp-spec?
  (VclampSpec (env hash-table?)
              (i       integer?)
              (sysname symbol?)
              (dflt-concs (lambda (x) (every default-conc? x)))
              (hboxn   integer?)
              (name    symbol?)
              (vchold  symbol?)
              (vcbase  symbol?)
              (vcinc   symbol?)
              (vcsteps symbol?)
              (vchdur  symbol?)
              (vcbdur  symbol?))

  (CnVclampSpec  (env hash-table?)
                 (i       integer?)
                 (sysname symbol?)
                 (dflt-concs (lambda (x) (every default-conc? x)))
                 (hboxn   integer?)
                 (name    symbol?)
                 (vchold  symbol?)
                 (vcbase  symbol?)
                 (vcinc   symbol?)
                 (vcsteps symbol?)
                 (vchdur  symbol?)
                 (vcbdur  symbol?)
                 (cnion   symbol?)
                 (cnhold  symbol?)
                 (cnbase  symbol?)
                 (cninc   symbol?)
                 (cnsteps symbol?)
                 (cnout   symbol?))
  )

(define (vclamp-spec-conc? x)
  (cases vclamp-spec x
         (VclampSpec (env i sysname dflt-concs hboxn name 
                      vchold vcbase vcinc vcsteps vchdur vcbdur) #f)
         (CnVclampSpec (env i sysname dflt-concs hboxn name 
                        vchold vcbase vcinc vcsteps vchdur vcbdur
                        cnion cnhold cnbase cninc cnsteps cnout ) #t)
         ))

(define (vclamp-spec-name x)
  (cases vclamp-spec x
         (VclampSpec (env i sysname dflt-concs hboxn name 
                      vchold vcbase vcinc vcsteps vchdur vcbdur) name)
         (CnVclampSpec (env i sysname dflt-concs hboxn name 
                        vchold vcbase vcinc vcsteps vchdur vcbdur
                        cnion cnhold cnbase cninc cnsteps cnout ) name)
         ))

(define (vclamp-spec-env x)
  (cases vclamp-spec x
         (VclampSpec (env i sysname dflt-concs hboxn name 
                      vchold vcbase vcinc vcsteps vchdur vcbdur) env)
         (CnVclampSpec (env i sysname dflt-concs hboxn name 
                        vchold vcbase vcinc vcsteps vchdur vcbdur
                        cnion cnhold cnbase cninc cnsteps cnout ) env)
         ))

(define (vclamp-spec-index x)
  (cases vclamp-spec x
         (VclampSpec (env i sysname dflt-concs hboxn name 
                      vchold vcbase vcinc vcsteps vchdur vcbdur) i)
         (CnVclampSpec (env i sysname dflt-concs hboxn name 
                        vchold vcbase vcinc vcsteps vchdur vcbdur
                        cnion cnhold cnbase cninc cnsteps cnout ) i)
         ))

(define-syntax pp
  (syntax-rules ()
    ((pp indent val ...) (ppf indent (quasiquote val) ...))))


(define (hoc-cname sys v)
  (s+ (hoc-name v) "_" (hoc-name sys)))

(define (escape-str x) (s+ "\"" x "\""))

(define (hoc-name s)
  (let ((cs (string->list (->string s))))
    (let loop ((lst (list)) (cs cs))
      (if (null? cs) (string->symbol (list->string (reverse lst)))
	  (let* ((c (car cs))
		 (c1 (cond ((or (char-alphabetic? c) (char-numeric? c) (char=? c #\_)) c)
			   (else #\_))))
	    (loop (cons c1 lst) (cdr cs)))))))


	    
(define (const-val v)
  (and (nemo:quantity? v)
       (cases nemo:quantity v
	      (CONST (name value) value)
	      (else (error 'const-val "invalid constant" v)))))


(define (hoc-prelude indent)


(pp indent (#<<EOF

vgraphbox.intercept(0)

vec_sizes = tstop/dt + 1	// recorded traces are all this size

vce=new SEClamp(0)

dt = 0.001

proc vcrun() {
	
	vce.dur1 = vchdur
	vce.dur2 = vcbdur
	vce.dur3 = vchdur

	tstop=vce.dur1 + vce.dur2 + vce.dur3
	
	vce.amp1 = vchold
	vce.amp3 = vchold
	
	for j=0, vcsteps-1 {
		x=vcbase+j*vcincrement
		vce.amp2=x

		init()

		run()
		$o2.line(g[$1], dt)

                for i=0,$o2.size()-1 {
                    $o4.printf("%g %g\n", $o3.x[i], $o2.x[i])
                    }

		if (stoppedrun()) {
			break
		}
	}
}
EOF
))
)


(define (matlab-extendstruct indent)
  (pp indent
      (function f = extendstruct (s #\, flds))
#<<EOF
   for i = 1:2:(2*(size(flds)(2)))
       k=flds(i){1};
       v=flds(i+1){1};
       s.(k) = v;
   endfor
EOF
   (f = s #\;)
   (endfunction)
))


(define (matlab-vclamp-run indent indent+ env i sysname
                           default-concs hboxn name 
                           vchold vcbase vcinc vcsteps vchdur vcbdur)
                                           
  (let ((nsteps (inexact->exact (const-val (hash-table-ref env vcsteps)))))

    (pp indent
        (imports = struct () #\;)
        (defconcs = #\{ 
                 ,@(intersperse
                    (fold (lambda (x ax) 
                            (let ((cname (car x)))
                              (append (list (sprintf "\"~Ai\"" cname) 
                                            (sprintf "\"~Ao\"" cname))
                                      ax)))
                          '() default-concs) #\,)
                 #\;
                 ,@(intersperse
                   (fold (lambda (x ax) 
                           (let ((cn (cadr x)) (cnout (caddr x)))
                             (append (list (s+ sysname "_parameters." (matlab-name cn))
                                           (s+ sysname "_parameters." (matlab-name cnout)) )
                                     ax)))
                         '() default-concs) #\,)
                 #\} #\;))

    (pp indent (,(s+ sysname "_ilog") = ,(s+ sysname "_vclamp_fn") 
                ;; vchold,vchdur,vcbase,vcdur,vcinc,vcsteps
                ,(intersperse
                  (append
                   `(,(s+ "@" sysname "_vclamp_run1_odepkg") 
                     dt ,(s+ sysname "_parameters") imports defconcs )
                   (map (lambda (x) (s+ sysname "_parameters." (matlab-name x)))
                        (list vchold vchdur vcbase vcbdur vcinc vcsteps)))
                  #\, ) #\; ))
    
    (pp indent (subplot(,hboxn #\, 3 #\, ,i) #\;))
    
    (pp indent (plot ,(intersperse
                       (list-tabulate
                        nsteps
                        (lambda (i) 
                          (s+ (s+ sysname "_ilog{" (+ 1 i) "}(:,1)") #\,  
                              (s+ (s+ sysname "_ilog{" (+ 1 i) "}")
                                  (s+ "(:,i_" name "_index)")))))
                       #\,) #\;))
    
    (pp indent (,(s+ name "_log") = vertcat ,(intersperse 
                                              (list-tabulate
                                               nsteps
                                               (lambda (i) 
                                                 (s+ #\[ 
                                                     (s+ sysname "_ilog{" (+ 1 i) "}(:,1)") #\,
                                                     (s+ sysname "_ilog{" (+ 1 i) "}")
                                                     (s+ "(:,i_" name "_index)")
                                                     #\])))
                                              #\, )
                #\;))
    
    (pp indent (save -ascii 
                     ,(escape-str (s+ name ".dat")) 
                     ,(s+ name "_log") #\;))
    ))


(define (matlab-cnvclamp-run indent indent+ env i sysname default-concs
                             hboxn name vchold vcbase vcinc vcsteps vchdur vcbdur
                             mod-ion-name cnhold cnbase cninc cnsteps cnout)

  (let ((ncnsteps (inexact->exact (const-val (hash-table-ref env cnsteps))))
        (nvcsteps (inexact->exact (const-val (hash-table-ref env vcsteps)))))

    (pp indent (defconcs = #\{ 
                 ,@(intersperse
                   (fold (lambda (x ax) 
                           (let ((cname (car x)))
                             (append (list (sprintf "\"~Ai\"" cname) 
                                           (sprintf "\"~Ao\"" cname))
                                     ax)))
                         '() default-concs) #\,)
                 #\;
                 ,@(intersperse
                   (fold (lambda (x ax) 
                           (let ((cn (cadr x)) (cnout (caddr x)))
                             (append (list (s+ sysname "_parameters." (matlab-name cn))
                                           (s+ sysname "_parameters." (matlab-name cnout)) )
                                     ax)))
                         '() default-concs) #\,)
                 #\} #\;))

    (pp indent (,(s+ sysname "_cnilog") = ,(s+ sysname "_cnvclamp_fn") 
                ;; vchold,vchdur,vcbase,vcdur,vcinc,vcsteps
                (,@(intersperse
                    (append
                     `(,(s+ "@" sysname "_vclamp_run1_odepkg")   
                       dt ,(s+ sysname "_parameters") defconcs )
                     (map (lambda (x) (s+ sysname "_parameters." (matlab-name x)))
                          (list vchold vchdur vcbase vcbdur vcinc vcsteps))
                     (list (s+ #\" mod-ion-name #\"))
                     (map (lambda (x) (s+ sysname "_parameters." (matlab-name x)))
                          (list cnhold cnbase cninc cnsteps cnout)))
                    #\,))
                #\;))
    
    (pp indent (subplot(,hboxn #\, 3 #\, ,i) #\;))
    
    (pp indent (plot( ,@(intersperse
                         (concatenate
                          (list-tabulate 
                           ncnsteps
                           (lambda (i)
                             (concatenate
                              (list-tabulate
                               nvcsteps
                               (lambda (j) 
                                 `(,(s+ sysname "_cnilog{" (+ 1 i) "," (+ 1 j) "}(:,1)")  
                                   ,(s+ (s+ sysname "_cnilog{" (+ 1 i) "," (+ 1 j) "}")
                                        (s+ "(:,i_" name "_index)"))
                                   ))
                               ))
                             )
                           ))
                         #\, )
                      )
                    #\;))
    
    
    (pp indent (,(s+ name "_" mod-ion-name "_log") = 
                vertcat ,(intersperse 
                          (concatenate
                           (list-tabulate 
                            ncnsteps
                            (lambda (i)
                              (list-tabulate
                               nvcsteps
                               (lambda (j) 
                                 (s+ #\[ 
                                     (s+ sysname "_cnilog{" (+ 1 i) "," (+ 1 j) "}(:,1)") #\,
                                     (s+ sysname "_cnilog{" (+ 1 i) "," (+ 1 j)  "}")
                                     (s+ "(:,i_" name "_index)")
                                     #\]))
                               ))
                            ))
                          #\, )
                #\;))
    
    (pp indent (save -ascii 
                     ,(escape-str (s+ name "_" mod-ion-name  ".dat")) 
                     ,(s+ name "_" mod-ion-name "_log") #\;))
    ))
  



(define (matlab-vclamp-run1-odepkg indent indent+ sysname octave-method )

  (pp indent (function ys = ,(s+ sysname "_vclamp_run1_odepkg (dt, params, imports, vchold, vchdur, vcbase, vcdur, vc)")))
                             
  (pp indent+ (global reltol abstol))
                             
  (pp indent+ (P = odeset (,(escape-str "RelTol") #\, reltol #\, 
                           ,(escape-str "AbsTol") #\, abstol #\, 
                           ,(escape-str "MaxStep") #\, 1 #\, 
                           ,(escape-str "InitialStep") #\, dt) #\;))
                             
  (pp indent+ (t0 = 0.0 #\;)
      (t1 = t0+vchdur #\;)
      (t2 = t1 #\;)
      (t3 = t2+vcdur #\;)
      (t4 = t3 #\;)
      (t5 = t4+vchdur #\;))
  
  (pp indent+ (vc = vchold #\;)
      (vinit = -65 #\;)
      (y0 = ,(s+ sysname "_init (vinit, params, imports)"))
      (sol = ,octave-method (,(s+ "@" sysname "_dy_vclamp") #\, 
                             #\[ t0 t1 #\] #\, y0 #\, P #\, params #\, imports #\, vc) #\;)
      (ys = #\[ sol.x sol.y #\] #\;)
      )
                             
  (pp indent+ (vc = vcbase #\;)
      (y0 = sol.y(size(sol.y)(1) #\, #\:)#\' #\; )
      (sol = ,octave-method (,(s+ "@" sysname "_dy_vclamp") #\, 
                             #\[ t2 t3 #\] #\, y0 #\, P #\, params #\, imports #\, vc) #\;)
      (ys = vertcat (ys #\, #\[ sol.x sol.y #\]) #\;))
                             
  (pp indent+ (vc = vchold #\;)
      (y0 = sol.y(size(sol.y)(1) #\, #\:) #\' #\; )
      (sol = ,octave-method (,(s+ "@" sysname "_dy_vclamp") #\, 
                             #\[ t4 t5 #\] #\, y0 #\, P #\, params #\, imports #\, vc) #\;)
      (ys = vertcat (ys #\, #\[ sol.x sol.y #\]) #\;))
  
  (pp indent (endfunction #\;))
  
  )



(define (matlab-vclamp-run1-dae indent indent+ sysname )

  (pp indent (function ys = ,(s+ sysname "_vclamp_run1_dae (dt, params, imports, vchold, vchdur, vcbase, vcdur, vc)")))
                             
  (pp indent+ (global reltol abstol))
                             
  (pp indent+ 
      (daspk_options (,(escape-str "absolute tolerance") #\, abstol) #\;)
      (daspk_options (,(escape-str "relative tolerance") #\, reltol) #\;)
      (daspk_options (,(escape-str "initial step size") #\, dt) #\;)
      (daspk_options (,(escape-str "maximum step size") #\, 1) #\;)
      (daspk_options (,(escape-str "maximum order") #\, 2) #\;)
      )
                             
  (pp indent+ (t0 = 0.0 #\;)
      (t1 = t0+vchdur #\;)
      (t2 = t1 #\;)
      (t3 = t2+vcdur #\;)
      (t4 = t3 #\;)
      (t5 = t4+vchdur #\;))
  
  (pp indent+ 
      (vc = vchold #\;)
      (vinit = -65 #\;)
      (y0 = ,(s+ sysname "_init (vinit, params, imports)"))
      (y0dot = ,(s+ sysname " (t0, y0, params, imports)"))
      (T1 = linspace (t0 #\, t1 #\, (t1-t0)/dt) #\' #\;)
      (fcn = @(y #\, ydot #\, t) ,(s+ sysname "_dy_vclamp") (t #\, y #\, params #\, imports #\, vc) #\;)
      (#\[ y ydot tout #\] = daspk (fcn #\, y0 #\, y0dot #\, T1) #\;)
      (ys = #\[ T1 y #\] #\;)
      )
                             
  (pp indent+
      (vc = vcbase #\;)
      (y0 = y (size (y) (1) #\, #\:) #\' #\;)
      (y0dot = ydot (size (y) (1) #\, #\:) #\' #\;)
      (T2 = linspace (t2 #\, t3 #\, (t3-t2)/dt) #\' #\;)
      (#\[ y ydot tout #\] = daspk (fcn #\, y0 #\, y0dot #\, T2) #\;)
      (ys = vertcat (ys #\, #\[ T2 y #\]) #\;)
      )
                             
  (pp indent+ 
      (vc = vchold #\;)
      (y0 = y (size (y) (1) #\, #\:) #\' #\;)
      (y0dot = ydot (size (y) (1) #\, #\:) #\' #\;)
      (T3 = linspace (t4 #\, t5 #\, (t5-t4)/dt) #\' #\;)
      (#\[ y ydot tout #\] = daspk (fcn #\, y0 #\, y0dot #\, T3) #\;)
      (ys = vertcat (ys #\, #\[ T3 y #\]) #\;)
      )
  
  (pp indent (endfunction #\;))
  
  )


(define (nemo:vclamp-translator sys . rest)

  (define (cid x)  (second x))
  (define (cn x)   (first x))
  
  (let-optionals rest ((target 'hoc) 
                       (filename #f)
                       (octave-method 'ode2r)
                       )

    (cases nemo:quantity (hash-table-ref sys (nemo-intern 'dispatch))  

	 (DISPATCH  (dis) 

	  (let ((imports  ((dis 'imports)  sys))
		(exports  ((dis 'exports)  sys))
                (defaults (nemo:defaults-query sys)))
	    
	    (let* ((indent        0)
		   (indent+       (+ 2 indent ))
		   
		   (sysname       (hoc-name ((dis 'sysname) sys)))
		   (filename      (or filename (s+ sysname "_vclamp" 
						   (case target ((matlab octave) ".m") (else ".hoc")))))

		   (consts        ((dis 'consts)  sys))
		   (asgns         ((dis 'asgns)   sys))
		   (states        ((dis 'states)  sys))
		   (reactions     ((dis 'reactions) sys))
		   (rates         ((dis 'rates) sys))
		   (defuns        ((dis 'defuns)  sys))
		   (components    ((dis 'components) sys))
		   
		   (vc-components (filter-map (lambda (x) (and (eq? 'voltage-clamp (second x)) x)) components))
                   (vcn           (length vc-components))
                   (hboxn         (inexact->exact (ceiling (/ vcn 3))))

		   (g             (match-let (((state-list asgn-list g) ((dis 'depgraph*) sys))) g))
		   (poset         (vector->list ((dis 'depgraph->bfs-dist-poset) g)))
		   
		   (gate-complex-info    (nemo:gate-complex-query sys))
		   (gate-complexes       (lookup-def 'gate-complexes gate-complex-info))
                   (mod-ions             (lookup-def 'mod-ions gate-complex-info))
                   (perm-ions            (lookup-def 'perm-ions gate-complex-info))
                   (epools               (lookup-def 'pool-ions gate-complex-info))
                   (unresolved-imports   (nemo:resolve-imports defaults imports exports epools))

                   (vc-specs
                    (fold
                     (lambda (comp i ax) 
                       (let* ((name (first comp))
                              (sym  (third comp))
                              (env          ((dis 'component-env) sys sym))
                              (subcomps     ((dis 'component-subcomps) sys sym))
                              (dflt-concs   (let ((dflt-conc-subcomps
                                                       (filter (lambda (x) (eq? (car x) 'default-concentration))
                                                               subcomps)))
                                                  (map (lambda (x) (let ((label (second x)) (sym (third x)))
                                                                     (match-let (((in out) 
                                                                                  ((dis 'component-exports) sys sym)))
                                                                                (list label in out))
                                                                     ))
                                                       dflt-conc-subcomps)
                                                  ))
                              (dflt-imports (fold (lambda (x ax)
                                                    (let ((label (first x)))
                                                      (cons (list (second x) (string->symbol (s+ label 'i) ))
                                                            (cons (list (third x) (string->symbol (s+ label 'o))) ax))
                                                      ))
                                                  '() dflt-concs))
                              )

                         (if (and (zero? (length mod-ions)) (not (null? unresolved-imports)))
                             (let ((dflt-qs (map second dflt-imports)))
                               (if (not (every (lambda (x) (member (second x) dflt-qs)) unresolved-imports))
                                   (error 'nemo:vclamp-translator 
                                          (sprintf
                                           "voltage clamp component ~A has unresolved imports" name)
                                          unresolved-imports))
                               ))
                         
                         (match-let (((vchold vcbase vcinc vcsteps vchdur vcbdur . rest) 
                                      ((dis 'component-exports) sys sym)))
                                    
                                    (if (zero? (length mod-ions))

                                        (cons
                                         (VclampSpec env i sysname dflt-concs hboxn name 
                                                     vchold vcbase vcinc vcsteps vchdur vcbdur)
                                         ax)

                                        (if (not (= (length mod-ions) (quotient (length rest) 5)))
                                            (error 'nemo:vclamp-translator
                                                   (sprintf "voltage clamp component ~A does not have enough entries for modulating ions " name )
                                                   (map first mod-ions))

                                            (let recur ((rest rest) (ions mod-ions) (ax ax))
                                              (if (null? rest) ax
                                                  (let ((ion (car ions)))
                                                    (match-let (((cnhold cnbase cninc cnsteps cnout . rest1) rest))
                                                               (recur rest1 (cdr ions)
                                                                      (cons
                                                                       (CnVclampSpec
                                                                        env i sysname dflt-concs
                                                                        hboxn name vchold vcbase vcinc vcsteps vchdur vcbdur
                                                                        (first ion) cnhold cnbase cninc cnsteps cnout)
                                                                       ax
                                                                       ))
                                                               ))
                                                  ))
                                            ))
                                    ))
                       )
                       
                     '()
                     vc-components (list-tabulate vcn (lambda (i) (+ 1 i))))
                    )
                   )



	      (if (not (null? vc-specs))

		  
		  (case target 
		    
                    
                    ((matlab) 
                     (begin

                       (if (not octave-method)
                           (error 'nemo:vclamp-translator 
                                  "voltage clamp solving method for Octave is not set"))
                       
                       (with-output-to-file filename
                         (lambda ()
                           (let* (
                                  (defs         (s+ sysname "_defs"))
                                  (init         (s+ sysname "_init"))
                                  (params       (s+ sysname "_parameters"))
                                  (current-vars (map (lambda (comp) (s+ "i_" (first comp))) vc-components))
                                  )
                             
                             (pp indent (,defs = ,(escape-str (s+ sysname ".m")) #\;)
                                 (autoload (,(escape-str sysname ) #\, ,defs ) #\;)
                                 (autoload (,(escape-str init) #\, ,defs ) #\;)
                                 (autoload (,(escape-str params) #\, ,defs ) #\;)
                                 )
                             
                             (pp indent (,params #\;))

                             (matlab-extendstruct indent)
                             
                             ;; TODO: use the logistic function for voltage stepping 
                             (pp indent (function dy = ,(s+ sysname "_dy_vclamp (t, y, params, imports, vc)")))
                             
                             (pp indent+ 
                                 (v = y(1) #\;)
                                 (i_elec = (vc - v) * 1e3 #\;)
                                 (dy = ,sysname  (t #\, y #\, params #\, imports) #\;)
                                 (dy(1) = dy(1) + i_elec #\;)
                                 )
                             
                             (pp indent (endfunction))
                             
                             (pp indent (function is = ,(s+ sysname "_currents (y, params, imports)")))

                             (pp indent+ 
                                 (t = y(1) #\;)
                                 (s = y(2 #\: length(y)) #\;)
                                 (#\[ ,@(cons 'y1 current-vars) #\] = ,sysname (t #\, s #\, params #\, imports) #\;)
                                 )
                             
                             (pp indent+ (is = #\[ ,@(cons 't current-vars) #\] #\;))
                             
                             (pp indent (endfunction))
                             
                             (matlab-vclamp-run1-odepkg indent indent+ sysname octave-method )
                             (matlab-vclamp-run1-dae indent indent+ sysname )


                             (pp indent (function cnilog = ,(s+ sysname "_cnvclamp_fn") 
                                                  (vclamp_run1 #\, dt #\, params  #\, defconcs  #\,
                                                      vchold #\, vchdur #\, vcbase #\, vcdur #\, vcinc #\, vcsteps #\, 
                                                      cname #\, cnhold #\, cnbase #\, cninc #\, cnsteps #\, cnout )))
                             


                             (pp indent+ 
                                 (ys = cell(cnsteps #\, vcsteps) #\;)
                                 (cn = cnbase #\;)
                                 (cnint = sprintf ("\"%s%s\"" #\, cname #\, "\"i\"") #\;)
                                 (cnext = sprintf ("\"%s%s\"" #\, cname #\, "\"o\"") #\;)
                                 )

                             (pp indent+ (imports = struct ,(intersperse '(cnint cn cnext cnout) #\, ) #\;))

                             (pp indent+ (imports = extendstruct (imports #\, defconcs) #\;))

                             (pp indent+ (if (cnsteps  == 1)))
                             (pp (+ 2 indent+)
                                 (y = ,(s+ sysname "_vclamp_fn (vclamp_run1 , dt , params, imports, defconcs, vchold , vchdur , vcbase , vcdur , vcinc , vcsteps) ;"))
                                 (cnilog (1 #\, #\:) = y #\;)
                                 )
                             (pp indent+ (else))
                             (pp (+ 2 indent+) 
                                 (for i = 1 #\: cnsteps))
                             (pp (+ 4 indent+) 
                                 (y = ,(s+ sysname "_vclamp_fn") (vclamp_run1 #\, dt #\, params #\, imports #\, defconcs #\,
                                                                     vchold #\, vchdur #\, vcbase #\, vcdur #\, vcinc #\, vcsteps) #\; )
                                 (cnilog(i #\, #\:) = y #\;)
                                 (cn = cn + cninc #\;)
                                 (imports.(cnint) = cn #\;)
                                 )
                             (pp (+ 2 indent+) (endfor))

                             (pp indent+ (endif))

                             (pp indent (endfunction))


                             (pp indent (function ilog = ,(s+ sysname "_vclamp_fn") 
                                                  (vclamp_run1 #\, dt #\, params #\, imports #\, defconcs  #\, 
                                                      vchold #\, vchdur #\, vcbase #\, vcdur #\, vcinc #\, vcsteps)))
                             
                             (pp indent+ (imports = extendstruct (imports #\, defconcs) #\;))

                             (pp indent+ 
                                 (ys = cell(1 #\, vcsteps) #\;)
                                 (vc = vcbase #\;))
                             
                             (pp indent+ (for i = 1 #\: vcsteps))
                             (pp (+ 2 indent+) 
                                 (y = vclamp_run1 
                                    (dt #\, params #\, imports #\, 
                                        vchold #\, vchdur #\, vc #\, vcdur) #\;)
                                 (ys(i) = y #\;)
                                 (vc = vc + vcinc #\;))
                             (pp indent+ (endfor #\;))
                             
                             (pp indent+ (ilog = cell(vcsteps #\, 1) #\;))
                             (pp indent+ (for i = 1 #\: vcsteps))
                             (pp (+ 2 indent+)
                                 (ilogv = #\[#\] #\;)
                                 (n = size(ys "{i}" )(1) #\;))
                             (pp (+ 2 indent+) (for j = 1 #\: n))
                             (pp (+ 4 indent+) 
                                 (next = ,(s+ sysname "_currents") (ys "{i}"(j #\, #\:) #\, params #\, imports) #\;)
                                 (ilogv = vertcat (ilogv #\, next) #\;))
                             (pp (+ 2 indent+) (endfor #\; ))
                             (pp (+ 2 indent+) (ilog "{i}" = ilogv #\;))
                             (pp indent+ (endfor #\;))
                             
                             (pp indent (endfunction))

                             
                             (pp indent (global reltol abstol))
                             
                             (pp indent (reltol = 1e-7 #\;))
                             (pp indent (abstol = 1e-7 #\;))
                             (pp indent (dt = 0.001 #\;))

                             (for-each
                              (lambda (spec) 
                                (let (
                                      (name (vclamp-spec-name spec))
                                      (i    (vclamp-spec-index spec))
                                      )

                                  (pp indent ("##" ,(s+ "i_" name) plot))
                                  (pp indent (,(s+ "i_" name "_index") = ,(+ 1 i) #\;))
                                  (cases vclamp-spec spec
                                         (CnVclampSpec (env i sysname dflt-concs hboxn name 
                                                            vchold vcbase vcinc vcsteps vchdur vcbdur
                                                            cnion cnhold cnbase cninc cnsteps cnout )
                                                       (matlab-cnvclamp-run indent indent+ env i sysname dflt-concs
                                                                            hboxn name vchold vcbase vcinc vcsteps vchdur vcbdur
                                                                            cnion cnhold cnbase cninc cnsteps cnout))
                                         
                                         (VclampSpec (env i sysname dflt-concs hboxn name 
                                                          vchold vcbase vcinc vcsteps vchdur vcbdur)
                                                     (matlab-vclamp-run indent indent+ env i sysname dflt-concs hboxn name 
                                                                        vchold vcbase vcinc vcsteps vchdur vcbdur))
                                         )))
                              vc-specs)
                             ))
                         ))
                     )

	  ((hoc)
	   
	   (with-output-to-file filename
	     (lambda ()
	       (let* ((vcn   (length vc-components))
		      (hboxn (inexact->exact (ceiling (/ vcn 3)))))
		 
		 (pp indent
		     ("load_file(\"nrngui.hoc\")")
		     ("load_file(" ,(s+ #\" sysname ".hoc" #\" ) ")")
		     ("objref vce	// voltage clamp")
		     ("objref g[" ,vcn "] // graph objects"))
		 
		 (pp indent
		     ("objref vgraphbox, hgraphbox[" ,hboxn "]")
		     ("vgraphbox=new VBox()")
		     ("vgraphbox.intercept(1)"))
		 
		 (let recur ((a 0) (b (min (- vcn 1) 2)) (hbi 0))
		   (pp indent
		       ("hgraphbox[" ,hbi "]=new HBox()")
		       ("hgraphbox[" ,hbi "].intercept(1)"))
		   
		   (if (< a b)
		       (pp indent ("for i=" ,a "," ,b " {"))
		       (pp indent ("i = " ,a)))
		   (pp (if (< a b) indent+ indent)
		       ("g[i]=new Graph()")
		       ("g[i].exec_menu(\"Keep Lines\")"))
		   (if (< a b) (pp indent ("}")))
		   (pp indent
		       ("hgraphbox[" ,hbi "].intercept(0)")
		       ("hgraphbox[" ,hbi "].map()"))
		   (if (> hboxn (+ 1 hbi))
		       (recur (+ b 1) (min (- vcn 1) (+ b 3)) (+ 1 hbi)))
		   )
		 
		 (hoc-prelude indent)
		 
		 (for-each 
		  (lambda (comp i) 
		    
		    (let ((name (first comp))
			  (sym  (third comp)))
		      
		      (match-let (((vchold vcbase vcinc vcsteps vchdur vcbdur . rest) 
                                   ((dis 'component-exports) sys sym)))
				 
				 (pp indent ("print \"generating " ,name "\""))
				 
				 (pp indent
				     ("vchold"      = ,(s+ "soma." (hoc-cname sysname vchold)))
				     ("vcbase"      = ,(s+ "soma." (hoc-cname sysname vcbase)))
				     ("vcincrement" = ,(s+ "soma." (hoc-cname sysname vcinc)))
				     ("vcsteps"     = ,(s+ "soma." (hoc-cname sysname vcsteps)))
				     ("vchdur"      = ,(s+ "soma." (hoc-cname sysname vchdur)))
				     ("vcbdur"      = ,(s+ "soma." (hoc-cname sysname vcbdur)))
				     )
				 
				 (pp indent
				     ("objref " ,name)
				     (,name = "new Vector(vec_sizes)")
				     (,(s+ name ".record") ,(s+ "(&soma.i_" name "_" sysname "( 0.5 ))"))
				     ("objref tlog")
				     ("tlog = new Vector(vec_sizes,0)")
				     ("tlog.record (&t)"))
				 
				 (pp indent ("objref logfile")
				     ("logfile = new File()")
				     ("logfile.wopen (" ,(s+ "\"" name ".dat\"") ")"))
				 
				 (pp indent ("vcrun(" ,i ", " ,name  ", tlog, logfile)")
				     ("g[" ,i "].label(.5,.85,\"" ,name "\")")
				     "logfile.close()")
				 )))
		  vc-components 
		  (list-tabulate (length vc-components) (lambda (i) i)))
		 
		 (if (> vcn 1) (pp indent ("for i=0, " ,(- vcn 1) " {")) (pp indent "i=0"))
		 (pp (if (> vcn 1) indent+ indent) ("g[i].exec_menu(\"View = plot\")"))
		 (if (> vcn 1) (pp indent ("}")))
		 (pp indent ("vgraphbox.map()"))
		 
		 ))
	     ))

  )))))
)))
)
