;;       
;; 
;; An extension for generating current pulse scripts from NEMO models.
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

(module nemo-iclamp

	(nemo:iclamp-translator)

	(import scheme chicken utils data-structures lolevel srfi-1 srfi-13 srfi-69)

	(require-extension lolevel datatype matchable strictly-pretty 
			   varsubst datatype 
			   nemo-core nemo-utils nemo-gate-complex)

(define-syntax pp
  (syntax-rules ()
    ((pp indent val ...) (ppf indent (quasiquote val) ...))))

(define (hoc-cname sys v)
  (s+ (hoc-name v) "_" (hoc-name sys)))

(define (hoc-name s)
  (let ((cs (string->list (->string s))))
    (let loop ((lst (list)) (cs cs))
      (if (null? cs) (string->symbol (list->string (reverse lst)))
	  (let* ((c (car cs))
		 (c1 (cond ((or (char-alphabetic? c) (char-numeric? c) (char=? c #\_)) c)
			   (else #\_))))
	    (loop (cons c1 lst) (cdr cs)))))))
			    
(define (matlab-name s)
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

(define (print-hoc-prelude indent)

(pp indent (#<<EOF

vgraphbox.intercept(0)

vec_sizes = tstop/dt + 1	// recorded traces are all this size

stim=new IClamp(0.5)

dt = 0.001

proc icrun() {
	
	stim.del = 1
	stim.dur = 0.1
	stim.amp = 10

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
EOF
))
)

(define (nemo:iclamp-translator sys . rest)

  (define (cid x)  (second x))
  (define (cn x)   (first x))
  
  (let-optionals rest ((target 'hoc) (filename #f))

    (cases nemo:quantity (hash-table-ref sys (nemo-intern 'dispatch))  

	 (DISPATCH  (dis) 

	  (let ((imports  ((dis 'imports)  sys))
		(exports  ((dis 'exports)  sys)))
	    
	    (let* ((indent        0)
		   (indent+       (+ 2 indent ))
		   
		   (sysname       (hoc-name ((dis 'sysname) sys)))
		   (filename      (or filename (s+ sysname "_iclamp" 
						   (case target
						     ((nest) ".sli")
						     ((matlab octave) ".m") 
						     (else ".hoc")))))

		   (consts        ((dis 'consts)  sys))
		   (asgns         ((dis 'asgns)   sys))
		   (states        ((dis 'states)  sys))
		   (reactions     ((dis 'reactions) sys))
		   (rates         ((dis 'rates) sys))
		   (defuns        ((dis 'defuns)  sys))
		   (components    ((dis 'components) sys))
		   
		   (vc-components (filter-map (lambda (x) (and (eq? 'voltage-clamp (second x)) x)) components))
		   (g             (match-let (((state-list asgn-list g) ((dis 'depgraph*) sys))) g))
		   (poset         (vector->list ((dis 'depgraph->bfs-dist-poset) g)))
		   
		   (gate-complex-info    (nemo:gate-complex-query sys))
		   (gate-complexes       (lookup-def 'gate-complexes gate-complex-info))
		   
		   (i-gates       (lookup-def 'i-gates gate-complex-info))
		   
		   )
	      
	      (if (not (null? vc-components))

		  
		  (case target 

		    ((nest)

		     (with-output-to-file filename
		       (lambda ()
			 (pp indent ("(mymodule) Install")
			     ("/neuron " ,(s+ #\/ sysname) " Create def")
			     ("/input /step_current_generator Create def")
			     ("/vlog /voltmeter Create def")
			     ("input neuron Connect")
			     ("vlog neuron Connect")
			     ("/step_current_parameters << /amplitude_times [ 1.0 ] /amplitude_values [ -10.0 ]  >> def")
			     ("input step_current_parameters SetStatus")
			     ("/vlog_parameters << /to_file true /to_memory false >> def")
			     ("vlog vlog_parameters SetStatus")
			     ("500.0 Simulate"))
			 )))

		    ((hoc)

		     (with-output-to-file filename
		       (lambda ()
			 (let* ((vcn   (length vc-components))
				(hboxn (inexact->exact (ceiling (/ vcn 3)))))
		       
			   (pp indent
			       ("load_file(\"nrngui.hoc\")")
			       ("load_file(" ,(s+ #\" sysname ".hoc" #\") ")")
			       ("objref stim")
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

			   (print-hoc-prelude indent)

			   (for-each 
			    (lambda (comp i) 
			      
			      (let ((name (first comp))
				    (sym  (third comp)))
				
				(match-let (((vchold vcbase vcinc vcsteps vchdur vcbdur) ((dis 'component-exports) sys sym)))

				    (pp indent ("print \"generating " ,name "\""))

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
				    
				    (pp indent ("icrun(" ,i ", " ,name  ", tlog, logfile)")
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
