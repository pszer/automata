(load "DFA.scm")

;;; evaluate one of these automata on a string by doing
;;; (dfa-compute AUTO '(a b c d ...))
;;; where '(a b c d ...) is the list string

;;; accepts regular language (01)*
(define d1
	(dfa-make '(q0 q1 fail)
	          '(0 1)
	          '((q0 0 q1) (q0 1 fail)
	            (q1 1 q0) (q1 0 fail)
	            (fail 0 fail) (fail 1 fail))
	          'q0
	          '(q0)))

;;; accepts regular language abcd
