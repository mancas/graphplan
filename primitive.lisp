;;;; FILE: primitive.lisp 
;;;; AUTHORS: Manuel Casas Barrado, Marcos Díez García
;;;; DESCRIPTION: Building Planification Graphs

;;;; - ABSTRACTION LAYER 0 - 

;;; TYPED DATA MECHANISM.
;;; Will allows us to distinguish the basic elements of the 
;;; Planification Graph.
;;; Basic elements: literal, predicate, action ...

;; Constructor. 
;; Creates an object with a label attached
(defun attach-type (typ contents)
  (cons typ contents))

;; Selectors. 
(defun typ (object)
  (car object))

(defun contents (object)
  (cdr object))

;; Functions over objects.

;; eq-type?
;; Checks if the 2 objs given have the same type, and the type is
;; equal to 'nametype'. Example: (eq-type? o1 o2 'literal)
(defun eq-type? (obj1 obj2 nametype)
  (and (equal (typ obj1) nametype)
    (equal (typ obj2) nametype)))


;;;; - ABSTRACTION LAYER 1 -

;;; LITERAL

;; Constructor.
;; Creates a literal for a given name ('A, 'B, 'ManosLimpias, ...)
;; and a given logical value (t or nil)
;; Example: (make-lit 'A nil)
(defun make-lit (name val)
  (attach-type 'literal
	       (cons name val)))

;; Selectors.

;; name-lit
;; Returns the symbolic name of the literal.
(defun name-lit (l)
  (car (contents l)))

;; val-lit
;; Returns the logical value of a given literal.
(defun val-lit (l)
  (cdr (contents l)))

;; Functions over literals.

;; lit?
;; Checks if the object is a literal.
(defun lit? (obj)
  (equal (typ obj)
	 'literal))

;;; PREDICATE

;; Constructor.
(defun make-pred (name obj-list val)
  (attach-type 'predicate
	       (cons `(,name ,obj-list)
		     val)))

;; Selectors.

;; name-pred
;; Returns the symbolic name of a given predicate.
(defun name-pred (p)
  (caar (contents p))) 

;; objs-pred
;; Returns the terms of a given predicate.
(defun objs-pred (p)
  (cadar (contents p))) 

;; val-pred
;; Returns the logical value of a given predicate.
(defun val-pred (p)
  (cdr (contents p))) 

;; Functions over predicates

;; pred?
;; Checks if the object is a predicate.
(defun pred? (obj)
  (equal (typ obj)
	 'predicate))

;;; VARIABLE

;; Constructor.
(defun make-var (name)
  (attach-type 'variable
	       `(,name)))
 
;; Selectors.
(defun name-var (v)
  (car (contents v)))

(defun var? (obj)
  (equal (typ obj)
	 'variable))

;;; CONJUNCTION

;; Constructor.
(defun conj (obj-list)
  (attach-type 'conjuncion
	       obj-list))

;; Functions over conjunctions.

;; nth-conj
;; Returns the 'nth' element of a given conjunction.
(defun nth-conj (conj n)
  (if (= n 0) 
    (nth n conj)
    (car
      (nth n conj))))

;;; NOT (predicates and literals)

(defun not-obj (obj)
  (cond
    ((lit? obj)
     (make-lit (name-lit obj) 
	       (not (val-lit obj))))
    ((pred? obj)
     (make-pred (name-pred obj) 
		(objs-pred obj) 
		(not (val-pred obj))))))

;;;; - ABSTRACTION LAYER 2 -

;;; STATE

;; Constructor.
;; Returns a 'state' for a given conjunction.
(defun make-state (name conj)
  (attach-type 'state
	       (cons name
		     (contents conj))))

;; Selectors.
(defun name-state (state)
  (car (contents state)))

(defun objs-state (state)
  (let ((objs (cdr (contents state)))
	(len (length 
	       (cdr (contents state)))))
    (if (= len 1)
      (car objs) ; remove extra '(' ')'
      objs)))

;; Functions over states
(defun state? (obj)
  (equal (typ obj) 'state))

;;; ACTION

;; Constructor.

(defun make-action (name preconditions effects)
  (attach-type 'action
	       `(,name ,(conj preconditions) ,(conj effects))))

;; Selectors.

;; pres
;; Returns the preconditions of a given action.
(defun pres (action)
  (contents (nth 2 action)))

;; effs
;; Returns the effects of a given action.
(defun effs (action)
  (contents (nth 3 action)))

;; Functions over actions.

(defun action? (obj)
  (equal (typ obj) 'action))

;; persistence? 
;; Checks if the given action is a 'persistence action'.
(defun persistence? (a)
  (equal (pres a) (effs a)))


;;; UNIFICATION
;;; This is a simplified version of unification. We restrict ourselves
;;; to literals and predicates, we will not consider variables.

;; Unification for literals.
;; Two given literals will 'unify' if and only if their names and their
;; values are the same.

;; Unification for predicates.
;; Two given predicates will 'unify' if and only if their names, values
;; and terms are the same.
;; The order of the terms matters: 'p(A B)' is not the same as 'p(B A)'

;; applicable-action? 
;; Returns true if all the preconditions are found in the state given.
;; Testing by #'equal ensures that names, values and terms are the same.
(defun applicable-action? (state preconds)
  (let ((terms (objs-state state))
	(pre (car preconds)))
    (cond 
      ((equal preconds ())
       t)
      (T ; otherwise
	(let ((found (find pre terms :test #'equal))) 
	  (and found
	       (applicable-action? state (cdr preconds))))))))

;;; MUTEX & LINK
;;; Mutex represents the conflict between 2 literals, predicates or
;;; actions.
;;; Link represents that some literal or predicate satifies a specific
;;; precondition of an action. Also represents the effects generated by
;;; some action.

;; Constructor. 

(defun link (obj1 obj2 label)
  (attach-type label		; mutex or link
	       (cons obj1 obj2)))

;; Selectors.

;; source
;; Returns the first object of a given link.
(defun source (link)
  (let ((link-typ (typ link)))
    (if (or (equal link-typ 'link)
	    (equal link-typ 'mutex))
      (car (contents link))
      (error "Argument is not a 'link' nor 'mutex'."))))

;; target
;; Returns the second object of a given link.
(defun target (link)
  (let ((link-typ (typ link)))
    (if (or (equal link-typ 'link)
	    (equal link-typ 'mutex))
      (cdr (contents link))
      (error "Argument is not a 'link' nor 'mutex'."))))

(defun label (link)
  (let ((link-typ (typ link)))
    (if (or (equal link-typ 'link)
	    (equal link-typ 'mutex))
      link-typ
      (error "Argument is not a 'link' nor 'mutex'."))))

;; Functions to check conflicts.

;; Literals and predicates.

(defun opposite? (obj1 obj2)
  (cond
    ((eq-type? obj1 obj2 'literal)
     (and
       (not (eq (val-lit obj1) ; different values
		(val-lit obj2)))
       (equal (name-lit obj1) ; same names
	      (name-lit obj2))))
    ((eq-type? obj1 obj2 'predicate)
     (and
       (not (eq (val-pred obj1) ; different values
		(val-pred obj2)))
       (equal (name-pred obj1) ; same names
	      (name-pred obj2))
       (equal (objs-pred obj1) ; same objects
	      (objs-pred obj2))))
    (T (error "Wrong type of arguments."))))

;; Actions.

(defun conflict? (terms1 terms2)
   (let ((nxt-term (car terms1)))
     (cond
       ((or (equal terms1 '())
	    (equal terms2 '()))
	nil)
       ((find (not-obj nxt-term) 
	      terms2
	      :test #'equal)
	T)
       ;otherwise
       (T (conflict? (cdr terms1)
		     terms2)))))

;; interference?
;; Returns 'true' if exists an interference conflict between 2 actions.
(defun interference? (action1 action2)
  (cond 
    ((conflict? (effs action1)
		(pres action2))
     T)
    ((conflict? (effs action2)
		(pres action1))
     T)
    (T nil)))

;; Competing needs conflict
(defun inconsistency-pres? (action1 action2)
  (conflict? (pres action1)
	     (pres action2)))

;; Inconsistency conflict
(defun inconsistency-effs? (action1 action2)
  (conflict? (effs action1)
	     (effs action2)))

;;;; - ABSTRACTION LAYER 3 -

;;; LAYERS

;; Constructors.

(defun make-action-layer (actions mutexes links)
  (attach-type 'action-layer
	       (list actions mutexes links)))

(defun make-state-layer (state mutexes links)
  (attach-type 'state-layer
	       (list state mutexes links)))

;; Functions over layers.

(defun action-layer? (layer)
  (equal (typ layer) 'action-layer))

(defun state-layer? (layer)
  (equal (typ layer) 'state-layer))

;; Selectors.

(defun actions (layer)
  (if (action-layer? layer)
    (car (contents layer))
    (error "Wrong type of layer.")))

(defun state (layer)
  (if (state-layer? layer)
    (car (contents layer))
    (error "Wrong type of layer.")))

(defun mutexes (layer)
  (cadr (contents layer)))

(defun links (layer)
  (caddr (contents layer)))


;;;; - ABSTRACTION LAYER 4 -

;;; Generators.

;; gen-persistent-actions
;; Returns the list of persistent actions for a given state.
;; We will use as the name of each action the name of the term in the
;; state. Example:
;;	term -> p
;;	persistent-action -> name: p, pres: {}, effs: {}
(defun gen-persistent-actions (terms)
  (let ((term (car terms))) 
    (cond 
      ((equal terms ())
       ())
      ((lit? term)
       (cons (make-action (name-lit term) () ())
	     (gen-persistent-actions (cdr terms))))
      ((pred? term)
       (cons (make-action (name-pred term) () ())
	     (gen-persistent-actions (cdr terms)))))))


;;;(defun gen-nxt-layer (current-layer previous-layer actions)
;;; )

;;;; ------------------------------------------------------------
;;;;			T E S T

; uncomment next line to load the 'tests' file
(load "tests.lisp")
