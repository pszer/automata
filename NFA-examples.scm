(load "NFA.scm")

;;; Evaluate one of these automata on a string by doing
;;; (nfa-compute AUTO '(a b c d ...))
;;; where '(a b c d ...) is the list string and AUTO the automata.

;;; Recognises regular language 01
(define n1
	(nfa-make '(start q0 q1)
	          '(0 1)
	          '((start 0 q0) (q0 1 q1))
	          'start
	          '(q1)))

;;; Recognises regular language (0^2i)U(0^3i) where i >= 1
(define n2
	(nfa-make '(branch a0 a1 b0 b1 b2)
	          '(0 1)
	          '((branch empty a0) (branch empty b0)
	            (a0 0 a1) (a1 0 a0)
	            (b0 0 b1) (b1 0 b2) (b2 0 b0))
	          'branch
	          '(a0 b0)))

;;; (substring-dfa alphabet sub)
;;; Creates an NFA that accepts a string with non-empty substring 'sub'
;;; of a given alphabet.
;;; i.e. automata for regular expression  Σ*'sub'Σ* .
;;; It is analogous to substring-dfa in "DFA-examples.scm", but
;;; the simplicity of this NFA compared to it's DFA is obvious,
;;; especially for a very long substring.
(define (substring-nfa alphabet sub)
	(define (make-interval a b)
		(if (> a b)
			'()
			(cons a (make-interval (+ a 1) b))))
	(define (make-states)
		;             start    success          substring checking states
		(append (list   0   (length sub)) (make-interval 1 (- (length sub) 1))))
	(define (make-nfa-trans)
		(define (stay-stuck state)
			(map (lambda (a) (trans-make state a state)) alphabet))
		(define (make-transition pos)
			(trans-make pos (list-ref sub pos) (+ pos 1)))
		(append (map make-transition (make-interval 0 (- (length sub) 1)))
		        (stay-stuck 0) (stay-stuck (length sub))))
	(nfa-make
		(make-states)
		alphabet
		(make-nfa-trans)
		0
		(list (length sub))))

;;; n-asdf-substr, example NFA that recognises strings with substring 'asdf'.
;;; regular expression is Σ*'asdf'Σ* .
(define eng-alphabet '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
(define n-asdf-substr
	(substring-nfa eng-alphabet '(a s d f)))
