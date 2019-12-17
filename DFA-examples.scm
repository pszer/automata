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

;;; (multiple-dfa n)
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

;;; (substring-dfa alphabet sub)
;;; Creates an automata that accepts a string with non-empty substring 'sub'
;;; of a given alphabet.
;;; i.e. automata for regular expression  Σ*'sub'Σ* .
;;;
;;; It works by having states 0, 1, 2, ..., |sub| . 0 is the start state, |sub|
;;; is the only success state.
;;; note. |sub| is the string length of sub
;;;
;;; If S is a state from 0 up to but not including |sub| (sub[i] is the i'th character)
;;; δ(S, q) = S+1 | if q  = sub[S]
;;;         = 0   | if q != sub[S]
;;;
;;; and
;;;
;;; δ(|sub|, q) = |sub| for any q ∈ Σ .
;;;
;;; The numeric state of the automata represents how many successive
;;; characters of 'sub' have been read, if a character that is not the
;;; successive character of 'sub' is read then the state goes back to 0.
;;; If |sub| successive characters of sub have been read, it means sub
;;; is a substring in the input string, so the state of the automata
;;; becomes forever stuck in an accept state (|sub|).
;;;
(define (substring-dfa alphabet sub)
	(define (make-states)
		(define (make-interval a b)
			(if (> a b)
				'()
				(cons a (make-interval (+ a 1) b))))
		;             start    success          substring checking states
		(append (list   0   (length sub)) (make-interval 1 (- (length sub) 1))))
	(define (make-dfa-trans)
		(define (advance-trans pos)
			(let ((sub-char (list-ref sub pos)))
				(define (make-trans-proc char)
					(if (equal? char sub-char)
						(trans-make pos char (+ pos 1))
						(trans-make pos char 0)))
				(map make-trans-proc alphabet)))
		(define (recur i result)
			(if (= i (length sub))
				result
				(recur (+ i 1) (append result (advance-trans i)))))
		(define (stay-stuck state)
			(map (lambda (a) (trans-make state a state)) alphabet))
		(append (recur 0 '())             ; substring checking transitions
				(stay-stuck (length sub)) ; stay-stuck in success state transitions
				(stay-stuck 'fail)))       ; stay stuck in fail state transitions
	(dfa-make
		(make-states)
		alphabet
		(make-dfa-trans)
		0
		(list (length sub))))

;;; d-asdf-substr, example automata that recognises strings with substring 'asdf'.
;;; regular expression is Σ*'asdf'Σ* .
(define eng-alphabet '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
(define d-asdf-substr
	(substring-dfa eng-alphabet '(a s d f)))
