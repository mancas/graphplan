;;;  TEST


;; literals

(setq P (make-lit 'P t))
(setq notp (make-lit 'P nil))
(setq q (make-lit 'Q t))
(setq notq (make-lit 'Q nil))


;; predicates

(setq pr1 (make-pred 'SOBRE '(A B) t))
(setq pr2 (make-pred 'SOBRE '(A B) nil))
(setq pr3 (make-pred 'SOBRE '(B A) nil))


;; actions

(setq a1 (make-action 'A1 (list p q) (list p q)))
(setq a2 (make-action 'A2 (list notq) (list p)))

(setq a3 (make-action 'A3 (list p q) '()))
(setq a4 (make-action 'A4 '() (list p q)))

(setq a5 (make-action 'A5 '() '()))
