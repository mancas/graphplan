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

;; gen-pairs
;; Returns the list of all the possible object pairs given the set of
;; objects.
(defun gen-pairs (objects)
  (let ((powerset (gen-powerset objects)))
    (loop for subset in powerset
	  if (= (length subset) 2)
	  collect subset)))

;; gen-powerset
;; Returns the list of all possible subsets of objects.
(defun gen-powerset (objects)
  (if (null objects)
    (list nil)
    (let ((prev (gen-powerset (cdr objects))))
      (append (mapcar #'(lambda (elt) (cons (car objects) elt)) 
		      prev)
	      prev))))

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
(defun make-state (&key (name '-) conj)
  (attach-type 'state
	       (cons name
		     (contents conj))))

;; Selectors.

(defun name-state (state)
  (car (contents state)))

(defun objs-state (state)
  (cdr (contents state)))

;; Functions over states.

(defun state? (obj)
  (equal (typ obj) 'state))

;; reach-target?
;; Returns 'true' if the target state is found in the current state,
;; given the terms of both states.
(defun reach-target? (current-terms target-terms)
  (let ((target-term (car target-terms)))

    (if (equal target-terms ())
      t
     (and (find target-term current-terms :test #'equal)
	 (reach-target? current-terms (cdr target-terms))))))

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
(defun applicable-action? (terms preconds)
  (let ((pre (car preconds)))
    (cond 
      ((equal preconds ())
       t)
      (T ; otherwise 
	(and (find pre terms :test #'equal)
	      (applicable-action? terms (cdr preconds)))))))

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


;; Auxiliar functions over mutexes.

;; link-all-to-all
;; When 2 actions have conflict we have to link all the effects of one
;; action with the effects of the other action. This function returns
;; a list with all those mutexes for those 2 actions.
(defun link-all-to-all (effs-pairs)
  (let ((effs-pair (car effs-pairs)))
    (if (equal effs-pairs ())
      ()
      (let ((eff1 (car effs-pair))
	    (eff2 (cadr effs-pair)))
	(cons (link eff1 eff2 'mutex)
	      (link-all-to-all (cdr effs-pairs)))))))

;; remov-duplicates
;; Returns the list of mutex without duplicates
;; Example:
;;	mutex1: (p, q)|
;;	mutex2: (q, p)| -----> result of filtering: (p, q)
;;	mutex3: (p, q)|
(defun remov-duplicates (unexplored)
  (let ((mutex (car unexplored)))
    (if (equal unexplored ())
      ()
      (if (or (find mutex 
		    (cdr unexplored) 
		    :test #'equal)
	      (find (link (target mutex) (source mutex) 'mutex)
		    (cdr unexplored)
		    :test #'equal))
	(remov-duplicates (cdr unexplored))
	(cons mutex
	      (remov-duplicates (cdr unexplored)))))))

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

;;; FUNCTIONS FOR GENERATION OF LAYERS

;; gen-persistent-actions
;; Returns the list of persistent actions for a given state.
;; We will use as the name of each action the name of the term in the
;; state. Example:
;;	term -> p
;;	persistent-action -> name: p, pres: {p}, effs: {p}
(defun gen-persistent-actions (terms)
  (let ((term (car terms))) 
    (cond
      ((equal terms ())
       ())
      ((lit? term)
       (cons (make-action (name-lit term) (list term) (list term))
	     (gen-persistent-actions (cdr terms))))
      ((pred? term)
       (cons (make-action (name-pred term) (list term) (list term))
	     (gen-persistent-actions (cdr terms)))))))

;; gen-new-state
;; Returns the set of the effects, which constitute the new state,
;; of the actions given.
(defun gen-new-state (actions)
  (let ((action (car actions)))
    (if (equal actions ())
      ()
      (remove-duplicates (append (effs action)
				 (gen-new-state (cdr actions)))
			 :test #'equal))))

;; link-state-to-actions
;; Returns the listing of links that connect a term of a state
;; with those actions that the term satisfies.
;; Example of use:
;;	(link-state-to-actions (objs-state st5) (list a5) (list a5))
;;
(defun link-state-to-actions (state actions)
  (link-state-to-actions* (objs-state state) actions actions))

(defun link-state-to-actions* (terms unexplored actions-record)
  (let ((term (car terms))
	(action (car unexplored)))
    (cond
      ((equal terms ())
       ())
      ((equal unexplored ()) ; all actions explored for term
       (link-state-to-actions* (cdr terms) ; next term
			       actions-record ; reset unexplored
			       actions-record))
      ((not (equal (find term (pres action) :test #'equal)
		       nil))
       (cons (link term action 'link)
	     (link-state-to-actions* terms ; keep current term as next
				     (cdr unexplored) ; next action
				     actions-record)))
      ; otherwise
      (T (link-state-to-actions* terms ; keep current term as next
				 (cdr unexplored) ; next action
				 actions-record)))))

;; link-actions-to-state
;; Returns the listing of links that connect an action with its
;; correspondant effects in the new state.
(defun link-actions-to-state (actions new-state)
  (let ((action (car actions)))
    (if (equal actions ())
      ()
      (append (link-actions-to-state* action (effs action) new-state)
	      (link-actions-to-state (cdr actions) new-state)))))

(defun link-actions-to-state* (action effs new-state)
  (let ((eff (car effs)))
    (if (equal effs ())
      ()
      (let ((found (find eff (objs-state new-state) :test #'equal)))
	(if (not (equal found nil))
	  (cons (link action found 'link)
		(link-actions-to-state* action (cdr effs) new-state))
	  (link-actions-to-state* action (cdr effs) new-state))))))


;; gen-actions-mutexes
;; Returns the list of mutexes between actions given 
;; the list of pairs of actions.
(defun gen-actions-mutexes (actions-pairs)
  (let ((actions-pair (car actions-pairs)))    
    (if (equal actions-pairs ())
      ()
      (let ((action1 (car actions-pair))
	    (action2 (cadr actions-pair)))
	(if (or (interference? action1 action2)
		(inconsistency-pres? action1 action2)
		(inconsistency-effs? action1 action2))
	  (cons (link action1 action2 'mutex)
		(gen-actions-mutexes (cdr actions-pairs)))
	  (gen-actions-mutexes (cdr actions-pairs)))))))

;; gen-opposite-terms-mutexes
;; Returns the list of mutexes between opposite terms, given the 
;; list of pairs of terms.
(defun gen-opposite-terms-mutexes (terms-pairs)
  (let ((terms-pair (car terms-pairs)))
    (if (equal terms-pairs ())
      ()
      (let ((term1 (car terms-pair))
	    (term2 (cadr terms-pair)))
	(if (opposite? term1 term2)
	  (cons (link term1 term2 'mutex)
		(gen-opposite-terms-mutexes (cdr terms-pairs)))
	  (gen-opposite-terms-mutexes (cdr terms-pairs)))))))

;; gen-conflict-terms-mutexes
;; Returns the list of mutexes between those pairs of terms produced
;; by conflictive actions.
(defun gen-conflict-terms-mutexes (actions-mutexes)
  (let ((actions-mutex (car actions-mutexes)))
    (if (equal actions-mutexes ())
      ()
      (let ((action1 (source actions-mutex))
	    (action2 (target actions-mutex)))
	(remov-duplicates
	  (append (link-all-to-all (gen-pairs (remove-duplicates
						(append (effs action1)
							(effs action2))
						:test #'equal)))
		  (gen-conflict-terms-mutexes (cdr actions-mutexes))))))))

;;;; - TOP ABSTRACTION LAYER -

;; gen-actions-layer
;; Returns for a given state-layer, the layer of actions available
;; (though not necessarily applicable) to that state.
(defun gen-actions-layer (state actions)
  (let* ((persistents (gen-persistent-actions (objs-state state)))
	 (mutexes (gen-actions-mutexes 
		    (gen-pairs (append actions persistents))))
	 (links (link-state-to-actions state
				       (append actions persistents))))
    (make-action-layer (append actions persistents)
		       mutexes
		       links)))

;; gen-state-layer
;; Returns for a given layer of actions, the new state layer produced
;; by those actions.
(defun gen-state-layer (actions-layer)
  (let* ((new-effs (gen-new-state
		     (actions actions-layer)))
	 (new-state (make-state :conj (conj new-effs)))
	 (mutexes (remove-duplicates ; action-conflictive and opposite terms
		    (append (gen-opposite-terms-mutexes
			      (gen-pairs (objs-state new-state)))
			    (gen-conflict-terms-mutexes
			      (mutexes actions-layer)))
		    :test #'equal))
	 (links (link-actions-to-state (actions actions-layer)
				       new-state)))
    (make-state-layer new-state mutexes links)))



;; make-graph
;; Returns the layers generated throughout the building of
;; the planification graph. Given an initial layer as the 'current-layer',
;; the 'target-layer' containing the target state, the set of actions of
;; the problem domain and the accumulator 'layers' containing
;; the initial layer.
(defun make-graphplan (current-layer target-layer actions layers)
  (if (or (reach-target? (objs-state (state current-layer))
			 (objs-state (state target-layer)))
	  (equal current-layer
		 (caddr layers))) ; previous state-layer
    layers
    (let* ((actions-layer (gen-actions-layer (state current-layer) actions))
	   (new-state-layer (gen-state-layer actions-layer)))
      (make-graphplan new-state-layer
		  target-layer
		  actions
		  (append new-state-layer actions-layer layers)))))

;;;; ------------------------------------------------------------
;;;;			T E S T

; uncomment next line to load the 'tests' file
(load "tests.lisp")
