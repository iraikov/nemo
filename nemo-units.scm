;;
;; NEMO units of measurement.
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


(module nemo-units

 (
  nemo:unitless
  nemo:basic-units
  nemo:unit?
  nemo:default-units
  nemo:quantity-name
  nemo:quantity-int
  nemo:unit-dims
  nemo:unit-scale
  )


 (import scheme chicken (only srfi-1 zip) srfi-69 data-structures)
 
 (require-extension unitconv)



(define-unit-prefix    milli second  ms)
(define-unit-prefix    milli volt    mV)
(define-unit-prefix    milli amp     mA)
(define-unit-prefix    pico  amp     pA)
(define-unit-prefix    nano  amp     nA)
(define-unit-prefix    micro amp     uA)
(define-unit-prefix    micro siemens uS)
(define-unit-prefix    milli siemens mS)
(define-unit-prefix    milli mole    mM)
 
(define-quantity   CurrentDensity        (/ Current Area))
(define-quantity   CapacitanceDensity    (/ Capacitance Area))
(define-quantity   ConductanceDensity    (/ Conductance Area))
(define-quantity   Resistivity           (* Resistance Length))
(define-quantity   ReactionRate1         (** Time -1))
(define-quantity   ReactionRate2         (* (** Substance -1) (** Time -1)))
(define-quantity   InversePotential      (** Potential -1))
(define-quantity   InversePotentialTime  (* (** Potential -1) (** Time -1)))

(define-unit milliamp-per-square-centimeter   CurrentDensity  (/ mA (* cm cm)) mA/cm2)
(define-unit microfarad-per-square-centimeter CapacitanceDensity (/ uF (* cm cm)) uf/cm2)
(define-unit siemens-per-square-centimeter    ConductanceDensity (/ S (* cm cm)) S/cm2)
(define-unit ohm.cm                           Resistivity     (* ohm cm) ohm.cm)

(define-unit degC    Temperature      1.0)
(define-unit /ms     ReactionRate1    1000.0)
(define-unit /mM-ms  ReactionRate2    1000000.0)
(define-unit /mV     InversePotential 1000.0)
(define-unit /mV-ms  InversePotentialTime 1000000.0)

(define nemo:basic-units
  (zip
   `(ms mV mA/cm2 pA nA uA mA mM uf/cm2 um S/cm2 uS mS ohm.cm ohm degC /ms /mM-ms /mV /mV-ms)
   (list ms mV mA/cm2 pA nA uA mA mM uf/cm2 um S/cm2 uS mS ohm.cm ohm degC /ms /mM-ms /mV /mV-ms)))
     
(define nemo:unitless unitless)

(define nemo:unit? unit?)

(define nemo:default-units
  (make-parameter
   (map cons 
        (list Time Potential CurrentDensity Current Substance 
              CapacitanceDensity Length ConductanceDensity Conductance
              Resistivity Resistance Temperature ReactionRate1 ReactionRate2 
              InversePotential InversePotentialTime)
        (list ms mV mA/cm2 nA mM 
              uf/cm2 um S/cm2 uS 
              ohm.cm ohm degC /ms /mM-ms 
              /mV /mV-ms)
        ))
  )


(define nemo:quantity-name quantity-name)
(define nemo:quantity-int quantity-int)
(define nemo:unit-dims unit-dims)


(define (nemo:unit-scale u)
  (let* (
         (defu (alist-ref (unit-dims u) (nemo:default-units) 
                          (lambda (x y)
                            (= (quantity-int x) (quantity-int y)))
                          ))
         )
    (and defu (unit-convert defu u))
    ))

)
