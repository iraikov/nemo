;;       
;; 
;; Procedures for querying default quantity values in NEMO models. 
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

(module nemo-defaults

 (nemo:defaults-query)

 (import scheme chicken srfi-1 srfi-13 srfi-69 data-structures)

 (require-extension matchable nemo-core nemo-utils)


(define (cid x)  (second x))
(define (cn x)   (first x))

(define $ string->symbol)

(define (isdefaults? x)
  (match x (('defaults name id) id) (('default name id) id) 
	 (else #f)))

(define (string-strip-prefix pre s)
  (let ((ss (->string s)))
    (if (string-prefix? pre ss)
	(string-drop ss (string-length pre)) ss)))


(define (nemo:defaults-query sys)
   (match-let ((($ nemo:quantity 'DISPATCH  dis) (hash-table-ref sys (nemo-intern 'dispatch))))
     (let recur ((comp-name (nemo-intern 'toplevel)) (ax (list)))
       (let ((subcomps  ((dis 'component-subcomps)  sys comp-name)))
	 (let-values (((defaults-comps other-comps)  (partition isdefaults? subcomps)))
		     (let ((defaults
			     (concatenate
			      (map (lambda (x) 
				     (let ((exports ((dis 'component-exports) sys (third x))))
				       (if (null? exports)
					   (nemo:error 'nemo:defaults-query 
						       ": defaults component " (third x)
						       " must export a quantity"))
				       (let ((prefix (s+ (third x) ":")))
					 (map (lambda (x) (list ($ (string-strip-prefix prefix x)) x)) exports))
				       ))
				  defaults-comps))
			     ))
		       (fold recur (append defaults ax) (map third other-comps))))))))
	  
)
