(load "DFA.scm")

;;; Evaluate one of these automata on a string by doing
;;; (dfa-compute AUTO '(a b c d ...))
;;; where '(a b c d ...) is the list string and AUTO the automata.

;;; Accepts regular language (01)*
(define d1
	(dfa-make '(q0 q1 fail)
	          '(0 1)
	          '((q0 0 q1) (q0 1 fail)
	            (q1 1 q0) (q1 0 fail)
	            (fail 0 fail) (fail 1 fail))
	          'q0
	          '(q0)))

;;; Accepts regular language ab(c)*
(define d2
	(dfa-make
		'(q0 qa qb qc fail)
		'(a b c)
		'((q0 a qa) (qa b qb) (qb c qc) (qc c qc) ; 'accept' transitions
		  (q0 b fail) (q0 c fail)
		  (qa a fail) (qa c fail)
		  (qb a fail) (qa b fail)
		  (qc a fail) (qc b fail))
		'q0
		'(qc qb)
		))

;;; Creates an automata that accepts binary numbers that are a multiple
;;; of a given number 'n'.
;;;
;;; It works by having states of numbers 0, 1, 2, 3, ..., n-1.
;;; The states represent the currently read binary number MODULO n
;;; Transition function is d(q,b) = | (q*2   MOD n) for b=0
;;;                                 | (q*2+1 MOD n) for b=1
;;; The binary number string is accepted if it 0 MODULO n (analogous
;;; to being a multiple of 'n').
(define (multiple-dfa n)
	(define (trans i result) ; iterates for states 0 up to n-1
		(if (= i n)
			result
			(trans (+ i 1) 
			(append (list (trans-make i 0 (remainder (* 2 i) n))
				          (trans-make i 1 (remainder (+ (* 2 i) 1) n)))
				    result))))
	(define (make-interval a b)
		(if (> a b)
			'()
			(cons a (make-interval (+ a 1) b))))
	(dfa-make
		(make-interval 0 (- n 1))
		'(0 1)
		(trans 0 '())
		0
		'(0)))
