;;; NFA.scm
;;; Non-Deterministic Finite-State Automata
;;;
;;; An NFA is a 5-tuple
;;; N = (Q,Σ,δ,s,F)
;;;
;;; Q is the set of states of D.
;;; Q can be any list of numbers/symbols, eg. '(q1 q2 q3)
;;;
;;; Σ is the alphabet of symbols/characters.
;;; Σ can be any list of numbers/symbols eg. '(a b c)
;;;
;;; ΣƐ is the alphabet + the empty string Ɛ 
;;; δ is the transition function, δ : Q x ΣƐ -> {Q}
;;; δ is implemented as a list of 3-tuples, (q,a,p).
;;; q is the current state, a is the character, p is the SET the next states.
;;; eg. '(q1 a {q2 q3}). q ∈ Q. a ∈ Σ . p∀ ∈ Q
;;; For an NFA, a state doesn't need a transition for every character in the
;;; alphabet, and it can have transitions for the empty string Ɛ .
;;;
;;; s is the starting state. s ∈ Q.
;;;
;;; F are the accept states. F ⊆ Q.
;;;
;;; Formal definition of computation with an NFA is as follows
;;; If N is a NFA, N is said to accept an input string 'w1, w2, ... , wn'
;;; if there exists a set of states 'r0, r1, r2, ..., rn' where
;;;  1. r0     = s (the start state).
;;;  2. rn     ∈ F (is an accept state).
;;;  3. r(i+1) ∈ δ(ri, w(i+1)) (states follow one of the paths of the transition
;;;                             function).
;;;
;;; In practive, you start with the SET of states of just {s}, then you repeat this process
;;; until you reach the end of the string :
;;;  1. Look if any of the current states have a transition function
;;;     on the empty string. If they do, have the new state set be
;;;     the union of itself and the sets of the empty string transitions
;;;     when step 1. is applied to them recursively (to traverse any possible multiple
;;;     empty string paths).
;;;  2. Read the next character in string,
;;;     i)  If non-empty, have the new set of states be the union
;;;         of the sets of the transition function when this character is applied
;;;         to each current state.
;;;     ii) If empty, check whether any of the current states are an accept state,
;;;         if at least one is, then the string is accepted, otherwise the string
;;;         is rejected.
;;;  
;;; In 2i), if the set of a transition function under a certain character is not defined
;;; then the set is the empty set by default. If the set of states becomes the empty set
;;; then the string gets rejected without needing to check any further states (as it will
;;; always be the empty set from there on out).
;;;
;;; Unlike in a Deterministic Finite-State Automaton, the current state isn't a single
;;; state but a set of states when simulating it, as long as one of the states in the
;;; set of states is an accept state the string gets accepted.
;;;
;;; NFA's are equal in 'power' to DFA's, in the sense that they both recognise only
;;; regular languages. Any NFA can be converted into a DFA, and the set of DFAs
;;; are a subset of NFAs.
;;;
;;; NOTE: THE SYMBOL FOR THE EMPTY STRING IS SIMPLY 'empty

(load "set.scm") ; set and symbol operation

;;; nfa constructor/selectors
;;; states = list
;;; alphabet = list
;;; transition = list
;;; start = symbol
;;; finish = list
(define (nfa-make states alphabet transition start finish)
	(define (test-finish) ; checks if finish ⊆ states
		(is-subset? finish states))
	(define (test-transition trans) ; checks if transition functions alphabet/states are correct.
		(if (null? trans) #t
			(let ((t (car trans)))
			     (if (and (is-element? (trans-state t) states)
			              (is-element? (trans-char  t) (adjoin-set 'empty alphabet))
			              (is-element? (trans-next  t) states))
			         (test-transition (cdr trans))
			         #f))))
	(define (test-start)
		(is-element? start states))
	(if (and (test-finish) (test-transition transition) (test-start))
		(list states alphabet transition start finish)
		(error "wrong finish states/invalid transition function")))

(define (nfa-states auto)
	(car auto))
(define (nfa-alphabet auto)
	(cadr auto))
(define (nfa-transition auto)
	(caddr auto))
(define (nfa-start auto)
	(cadddr auto))
(define (nfa-finish auto)
	(car (cddddr auto)))
(define (nfa-next-state nfa curr-states char)
	(define (union-of-states curr-states result)
		(if (null? curr-states)
			result
			(union-of-states (cdr curr-states)
			                 (union-set
			                         (next-state-set (nfa-transition nfa)
			                                         (car curr-states)
			                                         char '())
			                         result))))
	(define (next-state-set trans curr-state char result)
		(cond
			((null? trans)
				result)
			((and (equal? (trans-state (car trans)) curr-state)
			      (equal? (trans-char  (car trans)) char))
				(next-state-set (cdr trans) curr-state char (cons (trans-next (car trans)) result)))
			(else (next-state-set (cdr trans) curr-state char result))))
	(union-of-states curr-states '()))

(define (nfa-display auto)
	(display "States   Q : ") (display (nfa-states auto))
	(newline)
	(display "Alphabet Σ : ") (display (nfa-alphabet auto))
	(newline)
	(display "Function δ : ") (display (nfa-transition auto))
	(newline)
	(display "Start    q : ") (display (nfa-start auto))
	(newline)
	(display "Finish   F : ") (display (nfa-finish auto))
	(newline))

(define (trans-make state char next)
	(list state char next))
(define (trans-state t)
	(car t))
(define (trans-char t)
	(cadr t))
(define (trans-next t)
	(caddr t))

(define display-nfa-flag #f)
(define (nfa-compute automata string)
	(define (is-accept? states)
		(cond
			((null? states) #f)
			((is-element? (car states) (nfa-finish automata)) #t)
		    (else (is-accept? (cdr states)))))

	(define (handle-empty-strings states)
		(define (traverse-empty-strings states result)
			(define (is-empty-string? transition) (equal? (trans-char transition) 'empty))
			(define (get-empty-strings trans state result)
				(cond
					((null? trans) result)
					((and (is-empty-string? (car trans))
					      (equal? state (trans-state (car trans))))
						(get-empty-strings (cdr trans)
						                   state
						                   (adjoin-set (trans-next (car trans)) result)))
					(else (get-empty-strings (cdr trans) state result))))
			(if (null? states)
				result
				(let ((new-states (get-empty-strings (nfa-transition automata) (car states) '())))
					(traverse-empty-strings (cdr states) (union-set new-states result)))))

		(let ((new-states (traverse-empty-strings states '())))
			(if (is-subset? new-states states)
				states
				(handle-empty-strings (union-set states new-states)))))

	(define (compute nfa states str)
		(if display-nfa-flag (begin (display states) (newline)))
		(cond
			((null? states) #f)
			((null? str) (is-accept? states))
			(else (compute nfa
			         (nfa-next-state nfa (handle-empty-strings states) (car str))
			         ;(nfa-next-state nfa states (car str))
			         (cdr str)))))
	(compute automata (list (nfa-start automata)) string))


;;; Doesn't work if n1 and n2 have states
;;; that are the same.
(define (nfa-concatenate n1 n2)
	(define (create-concatenate-trans)
		(define (iter finish result)
			(if (null? finish)
				result
				(iter (cdr finish)
				      (cons (trans-make (car finish)
					                 'empty
					                 (nfa-start n2))
				             result))))
		(iter (nfa-finish n1) '()))
	(nfa-make
		(union-set (nfa-states n1) (nfa-states n2))
		(union-set (nfa-alphabet n1) (nfa-alphabet n2))
		(union-set (union-set (nfa-transition n1) (nfa-transition n2))
		           (create-concatenate-trans))
		(nfa-start n1)
		(nfa-finish n2)))
