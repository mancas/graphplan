
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
;; -----------------------------------------------
;;;	FIRST ABSTRACTION LAYER


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

;; -------------------------------------------------------

;; CONJUNCTION

;; constructor

(defun conj (obj-list)
  (attach-type 'conjuncion
	       obj-list))


;; functions over conjunctions

; nth-conj
;	returns the 'nth' element of a given conjunction.

(defun nth-conj (conj n)
  (if (= n 0) 
    (nth n conj)
    (car 
      (nth n conj))))

;; -------------------------------------------------------

;; NOT (predicates and literals)

(defun not-obj (obj)
  (cond
    ((lit? obj) (make-lit (name-lit obj) (not (val-lit obj))))
    ((pred? obj)(make-pred (name-pred obj) (objs-pred obj) (not (val-pred obj))))
    )
  )

;; -----------------------------------------------
;; -----------------------------------------------
;;;	SECOND ABSTRACTION LAYER

;; TODO: define state, action
(defun action (preconditions effects)
  (attach-type 'precondition
	       `(,(conj preconditions) ,(conj effects)))
 )

;;Get preconditions
(defun get-preconditions (action)
  (cdr (nth 1 action))
  )

;;Get effects
(defun get-effects (action)
  (cdr (nth 2 action))
 )