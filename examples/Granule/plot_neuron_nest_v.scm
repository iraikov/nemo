
(require-library srfi-1 irregex data-structures files posix extras ploticus cis)
(import
 (only srfi-1 filter)
 (only files make-pathname)
 (only posix glob)
 (only data-structures ->string alist-ref compose)
 (only extras fprintf)
 (only mathh cosh tanh log10)
 (prefix ploticus plot:)
 )


(define comment-pat (string->irregex "^#.*"))

(define (activity-plot nest-file neuron-file) ; stim-file

  (let* (
	 #;(stim-data
	  (fold
	   (lambda (data-file data)
	     (append
	      (map (lambda (line)  
                     (let ((strs (filter (lambda (x) (not (string-null? x))) (string-split  line " "))))
                       (map string->number strs)))
		   (filter (lambda (line) (not (irregex-match comment-pat line)))
			   (read-lines data-file)))
	      data))
	   '() (list stim-file)))

	 (nest-data
	  (fold
	   (lambda (data-file data)
	     (append
	      (map (lambda (line)  
                     (let ((strs (filter (lambda (x) (not (string-null? x))) (string-split  line "\t"))))
                       (map string->number strs)))
		   (filter (lambda (line) (not (irregex-match comment-pat line)))
			   (read-lines data-file)))
	      data))
	   '() (list nest-file)))

	 (neuron-data
	  (fold
	   (lambda (data-file data)
	     (append
	      (map (lambda (line) (map string->number (string-split  line " ")))
		   (filter (lambda (line) (not (irregex-match comment-pat line)))
			   (read-lines data-file)))
	      data))
	   '() (list neuron-file)))
         

	 #;(stim-data1
          (filter-map (lambda (lst) (and (pair? lst) (list (list-ref lst 0) (list-ref lst 1)))) stim-data))

	 (nest-data1
          (map (lambda (lst) (list (list-ref lst 1) (list-ref lst 2))) nest-data))

	 (neuron-data1
          (map (lambda (lst) (list (list-ref lst 0) (list-ref lst 1))) neuron-data))
	 )

    (let-values (
                 ((fd1 temp-path1) (file-mkstemp "/tmp/activity-plot.nest.XXXXXX"))
                 ((fd2 temp-path2) (file-mkstemp "/tmp/activity-plot.neuron.XXXXXX"))
;                 ((fd3 temp-path3) (file-mkstemp "/tmp/activity-plot.stim.XXXXXX"))
                 )
      
      (file-close fd1)
      (file-close fd2)
;      (file-close fd3)
      
      (let ((dataport (open-output-file temp-path1)))
        (for-each (lambda (x) (fprintf dataport "~A,~A~%" (car x) (cadr x))) nest-data1)
        (close-output-port dataport))
      
      (let ((dataport (open-output-file temp-path2)))
        (for-each (lambda (x) (fprintf dataport "~A,~A~%" (car x) (cadr x))) neuron-data1)
        (close-output-port dataport))

      #;(let ((dataport (open-output-file temp-path3)))
        (for-each (lambda (x) (fprintf dataport "~A,~A~%" (car x) (cadr x))) stim-data1)
        (close-output-port dataport))
      
      (plot:init 'eps (make-pathname "."  "nest_neuron_run.eps"))
      
      (plot:arg "-pagesize"   "20,30")
      (plot:arg "-textsize"   "14")
      (plot:arg "-maxrows"    "300000")
      (plot:arg "-maxfields"  "900000")
      (plot:arg "-maxvector"  "900000")

      (plot:proc "getdata"
                 `(
;		    ("showdata"   . "yes")
                   ("delim"      . "comma")
                   ("fieldnames" . "t1 v1")
                   ("pathname"   . ,temp-path1)
                   ))

      
      (plot:proc "areadef"
                 `(("title"     . "Granule Diwakar 2009 voltage trace (NEST simulation, soma)")
                   ("titledetails"     . "adjust=0,0.2")
                   ("rectangle" . "1 10 9 14")
                   ("areacolor" . "white")
                   ("xrange" . "0 3000")
                   ("yrange" . "-80 60")
                   ;("xaxis.label"     . "t [ms]")
                   ("xaxis.axisline"  . "yes")
                   ("xaxis.stubs"     . "inc")
                   ("yaxis.label"     . "V [mV]")
                   ("yaxis.labeldistance"     . "0.6")
                   ("yaxis.stubs"     . "inc")
                 ))
      
      (plot:proc "lineplot"
                 `(("xfield"    .  "t1")
                   ("yfield"    .  "v1")
                   ("linedetails" . "color=red width=0.5")
                   ))


      (plot:proc "getdata"
                 `(
;		    ("showdata"   . "yes")
                   ("delim"      . "comma")
                   ("fieldnames" . "t2 v2")
                   ("pathname"   . ,temp-path2)
                   ))

      
      (plot:proc "areadef"
                 `(("title"     . "Granule Diwakar 2009 voltage trace (NEURON simulation, soma)")
                   ("titledetails"     . "adjust=0,0.2")
                   ("rectangle" . "1 5 9 9")
                   ("areacolor" . "white")
                   ("xrange" . "0 3000")
                   ("yrange" . "-80 60")
                   ("xaxis.label"     . "t [ms]")
                   ("xaxis.labeldistance" . "0.6")
                   ("xaxis.axisline"  . "yes")
                   ("xaxis.stubs"     . "inc")
                   ("yaxis.label"     . "V [mV]")
                   ("yaxis.labeldistance"     . "0.6")
                   ("yaxis.stubs"     . "inc")
                 ))

      (plot:proc "lineplot"
                 `(("xfield"    .  "t2")
                   ("yfield"    .  "v2")
                   ("linedetails" . "color=blue width=0.5")
                   ))
		    
#;      (plot:proc "getdata"
                 `(
;		    ("showdata"   . "yes")
                   ("delim"      . "comma")
                   ("fieldnames" . "t i")
                   ("pathname"   . ,temp-path3)
                   ))

      
#;      (plot:proc "areadef"
                 `(("title"     . "Stimulus current")
                   ("titledetails"     . "adjust=0,0.2")
                   ("rectangle" . "1 3 8 4")
                   ("areacolor" . "white")
                   ("xrange" . "0 3000")
                   ("yrange" . "0 5")
                   ("xaxis.label"     . "t [ms]")
                   ("xaxis.axisline"  . "yes")
                   ("xaxis.stubs"     . "inc")
                   ("yaxis.label"     . "I [nA]")
                   ("yaxis.stubs"     . "inc")
                 ))

#;      (plot:proc "lineplot"
                 `(("xfield"    .  "t")
                   ("yfield"    .  "i")
                   ("stairstep"    .  "yes")
                   ("linedetails" . "color=green width=1")
                   ))
		    

      (plot:end)
      ))
    )

(apply activity-plot (command-line-arguments))


