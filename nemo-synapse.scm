;;       
;; 
;; Procedures for querying synaptic descriptions in NEMO models. 
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

(module nemo-synapse

 (nemo:post-synaptic-conductance-query)

 (import scheme chicken srfi-1 srfi-13 srfi-69)

 (require-extension matchable nemo-core nemo-utils)

(define (cid x)  (second x))
(define (cn x)   (first x))



(define (nemo:post-synaptic-conductance-query sys . rest)


  (match-let ((($ nemo:quantity 'DISPATCH  dis) (hash-table-ref sys (nemo-intern 'dispatch))))

    (let ((imports  ((dis 'imports)  sys))
	  (exports  ((dis 'exports)  sys)))

      (let* ((consts           ((dis 'consts)  sys))
	     (asgns            ((dis 'asgns)   sys))
	     (states           ((dis 'states)  sys))
	     (reactions        ((dis 'reactions) sys))
	     (rates            ((dis 'rates) sys))
	     (transients       ((dis 'transients) sys))
	     (defuns           ((dis 'defuns)  sys))
	     (components       ((dis 'components) sys))
             

	     (cond-synapses    
              (filter-map
               (match-lambda 
                ((name 'post-synaptic-conductance id) 
                 (list name id))
                (else #f)) 
               components))

	     (i-synapses
              (reverse
               (fold (lambda (cond-synapse ax) 
                       (match ((dis 'component-exports) sys (cid cond-synapse))
                              ((g erev wscale wthreshold)  (cons (list 'i g erev wscale wthreshold) ax))
                              ((g erev wscale)  (cons (list 'i g erev wscale #f) ax))
                              ((g erev)  (cons (list 'i g erev #f #f) ax))
                              (else (nemo:error 'post-synaptic-conductance
                                                "component does not export g and erev quantities"))))
                     (list) 
                     cond-synapses)))
             
	     (synapse-transients
              (reverse
               (fold (lambda (cond-synapse ax) 
                       (let ((syms ((dis 'component-symbols) sys (cid cond-synapse))))
                         (cons (filter (lambda (x) (member x transients)) syms) ax)))
                     (list) 
                     cond-synapses)))
             
             )
	
	`((post-synaptic-conductances ,cond-synapses)
	  (i-synapses ,i-synapses)
          (psc-transients ,synapse-transients)
	  ))
    )))

)
