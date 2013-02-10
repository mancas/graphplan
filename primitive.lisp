
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


;; functions over objects

; eq-type? .
;	checks if the 2 objs given have the same type, and the type is
;	equal to 'nametype'.
; 
; example: (eq-type? o1 o2 'literal)
;	
;
(defun eq-type? (obj1 obj2 nametype)
  (and (equal (typ obj1) (typ obj2)) (equal (typ obj1) nametype) (equal (typ obj2) nametype))
  )


;; -----------------------------------------------
;; -----------------------------------------------
;;;	FIRST ABSTRACTION LAYER


;; LITERAL

;; constructor

; make-lit .
;	creates a literal for a given name ('A, 'B, 'ManosLimpias, ...)
;	and a given logical value (t or nil)

; example:
;	(make-lit 'A nil)

(defun make-lit (name val)
  (attach-type 'literal 
	       (cons name val)))

;; selectors

; name-lit .
;	returns the name of a given literal

(defun name-lit (l) 
  (car (contents l)))

; val-lit .
;	returns the value of a given literal

(defun val-lit (l)
  (cdr (contents l)))

;; functions over literals

; lit? .
;	checks if the object is a literal.

(defun lit? (obj)
  (equal (typ obj)
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

(defun pred? (obj)
  (equal (typ obj)
	 'predicate))

;; -------------------------------------------------------

;; CONJUNCTION

;; constructor

(defun conj (obj-list)
  (attach-type 'conjuncion
	       obj-list))


;; functions over conjunctions

; nth-conj .
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
    ((lit? obj)
     (make-lit (name-lit obj) (not (val-lit obj))))
    ((pred? obj)
     (make-pred (name-pred obj) (objs-pred obj) (not (val-pred obj))))))

;; -----------------------------------------------
;; -----------------------------------------------
;;;	SECOND ABSTRACTION LAYER

;; STATE

;; constructor .
;	returns a 'state' for a given list of conjunctions.

(defun make-state (name conj)
  (attach-type 'state
	       (cons name
		     (contents conj))))

;; selectors

(defun name-state (state)
  (car (contents state)))

(defun objs-state (state)
  (let ((objs (cdr (contents state)))
	(len (length (cdr (contents state)))))
    
    (if (= len 1)
      (car objs) ; remove extra '(' ')'
      objs)))

;; functions over states

(defun state? (obj)
  (equal (typ obj)
	 'state))

;; ----------------------------------------------------------------

;; ACTION

;; constructor

(defun make-action (name preconditions effects)
  (attach-type 'action
	       `(,name ,(conj preconditions) ,(conj effects))))

;; selectors

; Get preconditions

(defun pres (action)
  (cdr (nth 2 action)))

; Get effects

(defun effs (action)
  (cdr (nth 3 action)))

;; functions over actions

(defun action? (obj)
  (equal (typ obj)
	 'action))

; persistence? . 
;	checks if the given action is a 'persistence action'

(defun persistence? (a)
  (equal (pres a)
	 (effs a)))

;; ----------------------------------------------------------------

;; MUTEX
;;	represents the conflict between 2 literals/actions.


;; constructor

(defun mutex (obj1 obj2)  
  (attach-type 'mutex
	       (cons obj1 obj2)))


;; Functions to check conflicts

;; literals and predicates.

(defun opposite? (obj1 obj2)
  (cond 
    ((eq-type? obj1 obj2 'literal)(and (not (eq (val-lit obj1) (val-lit obj2))) (equal (name-lit obj1) (name-lit obj2))))

    ((eq-type? obj1 obj2 'predicate) (and (not (eq (val-pred obj1) (val-pred obj2))) (equal (name-pred obj1) (name-pred obj2))))    
    (T (error "Wrong type of arguments."))))

;; actions.

;; Interference conflict

(defun interference? (action1 action2)
  (cond 
    ((has-interference (effs action1) (pres action2)) T)
    ((has-interference (effs action2) (pres action1)) T)
    (T nil)
    )
  )

(defun has-interference (effects preconditions)
  (cond
    ((or (equal 0 (length effects))(equal 0 (length preconditions)) (equal nil (car effects)) (equal nil (car preconditions))) nil)
    ((equal (car effects) (not-obj (car preconditions))) T)
    (T (has-interference (cdr effects) (cdr preconditions)))
    )
  )

;; Inconsistency
(defun inconsistency? (action1 action2)
  (cond
    ((has-inconsistency (effs action1) (effs action2)) T)
    ((has-inconsistency (effs action2) (effs action1)) T)
    (T nil)
    )
  )

(defun has-inconsistency (effects1 effects2)
  (cond
    ((or (equal 0 (length effects1))(equal 0 (length effects2)) (equal nil (car effects1)) (equal nil (car effects2))) nil);;Si alguna lista de efectos es vacia o contiene nil entonces no hay incosistencia
    ((opposite? (car effects1) (car effects2)) T)
    (T (has-inconsistency effects1 (cdr effects2)))
    )
  )