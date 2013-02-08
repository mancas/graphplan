
;;; - TYPED DATA MECHANISM -

;;; Will allows us to distinguish the basic elements of the 
;;; Planification Graph.
;;;
;;; Basic elements: literal, predicate, action ...

(defun attach-type (typ contents)
  (cons typ contents))

;; selectors =)

(defun typ (object)
  (car object))

(defun contents (object)
  (cdr object))

;; -----------------------------------------------

;; -LITERAL-

;; constructor

; make-lit
;	creates a literal for a given name ('A, 'B, 'ManosLimpias, ...)
;	and a given logical value (t or nil)
; example:
;	(make-lit 'A nil)

(defun make-lit (name val)
  (attach-type 'literal 
	       (cons name val)))

;; selectors

; name-lit
;	returns the name of a given literal

(defun name-lit (l) 
  (car (contents l)))

; val-lit
;	returns the value of a given literal
(defun val-lit (l)
  (cdr (contents l)))

;; functions over literals

; toggle-lit
;	changes the value of some literal to the opposite value.

(defun toggle-value (element)
  (cond
    ((lit? element) (make-lit (name-lit element) (not (val-lit element))))
    ((pred? element)(make-pred (name-pred element) (objs-pred element) (not (val-pred element))))
    )
  )

;lit?
;	checks if the object is a literal.
(defun lit? (object)
  (equal (typ object)
	 'literal))

;; -------------------------------------------------------

;; PREDICATE

;; constructor

(defun make-pred (name obj-list val)
  (attach-type 'predicate
	       (cons `(,name ,obj-list)
		     val)))

;; selectors

(defun name-pred (p)
  (caar (contents p))) ; take first-first

(defun objs-pred (p)
  (cadar (contents p))) ; take first-last-first

(defun val-pred (p)
  (cdr (contents p))) ; take last

;; functions over predicates

(defun pred? (p)
  (equal (typ p)
	 'predicate))



;;; -----------------------------------------------------------------
;;;	SECOND ABSTRACTION LAYER
;;;

(defun conj (obj-list)
  (attach-type 'conjuncion
	       obj-list))

; nth-conj
;	returns the 'nth' element of a given conjunction.
;
;(defun nth-conj (conj n)
;  (if (= n 0) 
;    (nth n conj)
;    (car 
;      (nth n conj))))
