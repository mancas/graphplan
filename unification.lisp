
(defconstant *fail* nil "Indicates unification failure")



;;; PRIMITIVE ELEMENTS

;; constructor binding
(defun make-binding (var val)
  (cons var val))

;; selectors 
(defun binding-val (binding)
 "Get the value part of a single binding."
      (cdr binding))

(defun binding-var (binding)
  "Get the variable part of a single binding."
      (car binding))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
      (assoc var bindings))

;; auxiliar functions
(defun extend (var val bindings)
  "Add a (var . value) pair to a binding list."
      (cons (make-binding var val) bindings))

;;; UNIFY

(defun unify (t1 t2 bindings)

  (cond ((equal t1 t2) bindings) ;; bindings initialy = '()
	((var? t1) (unify-var t1 t2 bindings))
	((var? t2) (unify-var t2 t1 bindings))



	
	
	)
 )



(defun unify-var (t1 t2 bindings)
  (cond ((occurs-in? t1 t2 bindings) *fail*)
	()

    
    
    )
  )

;; (occurs-in? ...)
