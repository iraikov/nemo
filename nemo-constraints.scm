;;       
;; 
;; Procedures for construction of constraint equation for biophysical quantities from NEMO models. 
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

(module nemo-constraints

 (
  nemo:constraint-definitions
  )

 (import scheme chicken srfi-1 srfi-13 srfi-69)
 
 (require-extension matchable nemo-core nemo-utils)
 
 (define (cid x)  (second x))
 (define (cn x)   (first x))

 (define (nemo:constraint-definitions 
          gate-complexes i-gates i-syns pscs marea imports
          state-power quantity-unit 
          component-exports component-subcomps
          quantity-name)

  (let* (
         (c-eqs.units
          (filter-map
           
           (lambda (gate-complex) 
             
             (let* (
                    (label             (first gate-complex))
                    (n                 (second gate-complex))
                    (subcomps          (component-subcomps n))
                    (acc               (lookup-def 'accumulating-substance subcomps))
                    (perm              (lookup-def 'permeating-ion subcomps))
                    (permqs            (and perm (component-exports (cid perm))))
                    (pore              (lookup-def 'pore subcomps))
                    (permeability      (lookup-def 'permeability subcomps))
                    (gates             (filter (lambda (x) (equal? (car x) 'gate)) subcomps))
                    (sts               (map (lambda (gate) (component-exports (cid gate))) gates))
                    )
               
               (if (and pore (null? permqs))
                   (nemo:error 'nemo:constraints ": ion channel definition " label
                               "permeating-ion component lacks exported quantities"))
               
               (for-each 
                (lambda (st)
                  (if (null? st)
                      (nemo:error 'nemo:constraints ": ion channel definition " label
                                  "gate component lacks exported quantities")))
                sts)
               
               (if (not (or pore permeability))
                   (nemo:error 'nemo:constraints ": ion channel definition " label
                               "lacks any pore or permeability components"))
               
               
               (cond ((and perm permeability (pair? gates))
                      (let* (
                             (pmax  (cadr (component-exports (cid permeability))))
                             (punit (quantity-unit pmax))
                             (pwrs  (map (lambda (st) (map (lambda (n) (state-power n)) st)) sts))
                             )
                        `((> ,pmax 0.0) (,pmax ,punit))
                        ))
                     
                     ((and perm pore (pair? gates))
                      (case (cn perm)
                        
                        ((non-specific)
                         (let* (
                                (gmax  (car (component-exports (cid pore))))
                                (gunit (quantity-unit gmax))
                                (pwrs  (map (lambda (st) (map (lambda (n) (state-power n)) st)) sts))
                                )
                        `((> ,gmax 0.0) (,gmax ,gunit))
                        ))
                        
                        (else
                         (let* (
                                (gmax  (car (component-exports (cid pore))))
                                (gunit (quantity-unit gmax))
                                (pwrs  (map (lambda (st) (map (lambda (n) (state-power n)) st)) sts))
                                )
                           `((> ,gmax 0.0) (,gmax ,gunit))
                           ))
                        ))
                     
                     ((and perm pore)
                      (case (cn perm)
                        ((non-specific)
                         (let* (
                                (e     (car permqs))
                                (eunit (quantity-unit e))
                                (gmax  (car (component-exports (cid pore))))
                                (gunit (quantity-unit gmax))
                                )
                           `((> ,gmax 0.0) (,gmax ,gunit))
                           ))
                        (else
                         (nemo:error 'nemo:ionic-currents ": invalid ion channel definition " label))))
                     
                     ((and acc pore (pair? gates))
                      (let* (
                             (gmax  (car (component-exports (cid pore))))
                             (gunit (quantity-unit gmax))
                             (pwrs  (map (lambda (st) (map (lambda (n) (state-power n)) st)) sts))
                             )
                        `((> ,gmax 0.0) (,gmax ,gunit))))

                     (else (nemo:error 'nemo:ionic-currents ": invalid ion channel definition " label))

                     ))
             )
           
           gate-complexes))

         (c-eqs  (if marea
                     (cons (list '> marea 0.0) (map first c-eqs.units))
                     (map first c-eqs.units)))

         (c-units (append (map second c-eqs.units)
                          (map (lambda (x) (list (first x) (quantity-unit (first x)))) 
                               imports)))

         )

    `((c-eqs ,c-eqs)
      (c-units ,c-units))

    ))

	  
)
