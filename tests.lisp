;;;  TEST

;; literals
(setq p (make-lit 'P t))
(setq notp (make-lit 'P nil))
(setq q (make-lit 'Q t))
(setq notq (make-lit 'Q nil))

;; predicates
(setq pr1 (make-pred 'SOBRE '(A B) t))
(setq pr2 (make-pred 'SOBRE '(A B) nil))
(setq pr3 (make-pred 'SOBRE '(B A) t))

;; conjunctions
(setq c1 (conj (list p q)))
(setq c2 (conj (list p q pr1)))
(setq c3 (conj (list p q pr2)))
(setq c4 (conj (list p q pr3)))

;; states
(setq st1 (make-state 'S1 c1))
(setq st2 (make-state 'S2 c2))
(setq st3 (make-state 'S3 c3))

;; actions
(setq a1 (make-action 'A1
		      (list p q) 
		      (list p q)))

(setq a2 (make-action 'A2
		      (list notq) 
		      (list p)))

(setq a3 (make-action 'A3
		      (list p q) 
		      ()))

(setq a4 (make-action 'A4 
		      ()
		      (list p q)))

(setq a5 (make-action 'A5
		      ()
		      ()))

(setq a6 (make-action 'A6
		      ()
		      (list notp)))

(setq a7 (make-action 'A7
		      (list notq)
		      ()))
;; mutexes
(setq m1 (link a1 a2 'mutex))
(setq m2 (link a2 a3 'mutex))

;; links
(setq l1 (link p a1 'link))
(setq l2 (link q a1 'link))
(setq l3 (link notq a2 'link))

;; layers
(setq layer1 (make-state-layer st1
			       ()
			       (list l1 l2)))

(setq layer2 (make-action-layer (list a1 a2)
				(list m1 m2)
				()))

