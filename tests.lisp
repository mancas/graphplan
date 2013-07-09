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
(setq c2 (conj (list p pr1)))
(setq c3 (conj (list p pr2 pr3)))
(setq c4 (conj (list p q pr3)))
(setq c5 (conj (list p)))
(setq c6 (conj (list notp)))
(setq c7 (conj (list q)))


;; states
(setq st1 (make-state :name 'S1 :conj c1))
(setq st2 (make-state :name 'S2 :conj c2))
(setq st3 (make-state :name 'S3 :conj c3))
(setq st5 (make-state :name 'S5 :conj c5))
(setq st6 (make-state :name 'S6 :conj c6))

;; actions
(setq a1 (make-action 'A1
		      (list p q) 
		      (list p q)))

(setq a2 (make-action 'A2
		      (list notq) 
		      (list p)))

(setq a3 (make-action 'A3
		      (list p notq) 
		      ()))

(setq a4 (make-action 'A4 
		      ()
		      (list q p)))

(setq a5 (make-action 'A5
		      (list p)
		      ()))

(setq a6 (make-action 'A6
		      (list pr2 pr3)
		      (list notp)))

(setq a7 (make-action 'A7
		      (list notq)
		      ()))
;; mutexes
(setq m1 (link a1 a2 'mutex))
(setq m2 (link a2 a3 'mutex))

(setq m3 (link p q 'mutex))
(setq m4 (link notq p 'mutex))
(setq m5 (link q p 'mutex))
(setq m6 (link p q 'mutex))


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

;; test1

(setq layer3  (gen-actions-layer st5
				 (list a5 a6)))

;; scenario 1

(setq initial-state (make-state :name 'initial :conj c5)) ; p
(setq target-state (make-state :name 'target :conj c7)) ; q

(setq action1 (make-action 'A1
		      (list p)
		      (list q)))

(setq action2 (make-action 'A2
		      ()
		      (list notp)))

(setq initial-layer (make-state-layer initial-state
				      ()
				      ()))
(setq target-layer (make-state-layer target-state
				      ()
				      ()))


