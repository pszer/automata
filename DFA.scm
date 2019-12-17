;;; DFA.scm
;;; Deterministic Finite-State Automata
;;;
;;; A DFA is a 5-tuple
;;; D = (Q,Σ,δ,s,F)
;;;
;;; Q is the set of states of D.
;;; Q can be any list of numbers/symbols, eg. '(q1 q2 q3)
;;;
;;; Σ is the alphabet of symbols/characters.
;;; Σ can be any list of numbers/symbols eg. '(a b c)
;;;
;;; δ is the transition function, δ : QxΣ -> Q
;;; δ is implemented as a list of 3-tuples, (q,a,p).
;;; q is the current state, a is the character, p is the next state.
;;; eg. '(q1 a q2). q ∈ Q. a ∈ Σ. p ∈ Q
;;; For a DFA, every state must have a defined transition for every character.
;;;
;;; s is the starting state. s ∈ Q.
;;;
;;; F are the accept states. F ⊆ Q.

;;; recognises if two numbers/symbols/lists of symbols or numbers are equal.
(define (equal? a b)
	(cond ((and (number? a) (number? b))
	          (= a b))
	      ((and (symbol? a) (symbol? b))
	          (eq? a b))
	      ((and (null? a) (null? b))
	          #t)
	      ((and (list? a) (list? b))
	          (and (equal? (car a) (car b))
	               (equal? (cdr a) (cdr b))))
	      (else
	          #f)))

;;; predicate for whether or not element is in set
(define (is-element? element set)
	(cond
		((null? set) #f)
		((equal? element (car set)) #t)
		(else (is-element? element (cdr set)))))
(define (is-subset? sub set)
	(if (null? sub)
		#t
		(if (is-element? (car sub) set)
			(is-subset?  (cdr sub) set)
			#f)))

;;; dfa constructor/selectors
(define (dfa-make states alphabet transition start finish)
	(define (test-finish) ; checks if finish ⊆ states
		(is-subset? finish states))
	(define (test-transition trans) ; checks if transition functions alphabet/states are correct.
		(if (null? trans) #t
			(let ((t (car trans)))
			     (if (and (is-element? (trans-state t) states)
			              (is-element? (trans-char  t) alphabet)
			              (is-element? (trans-next  t) states))
			         (test-transition (cdr trans))
			         #f))))
	(if (and (test-finish) (test-transition transition))
		(list states alphabet transition start finish)
		(error "wrong finish states/invalid transition function")))

(define (dfa-states auto)
	(car auto))
(define (dfa-alphabet auto)
	(cadr auto))
(define (dfa-transition auto)
	(caddr auto))
(define (dfa-start auto)
	(cadddr auto))
(define (dfa-finish auto)
	(car (cddddr auto)))

(define (trans-make state char next)
	(list state char next))
(define (trans-state t)
	(car t))
(define (trans-char t)
	(cadr t))
(define (trans-next t)
	(caddr t))

(define (dfa-compute automata string)
	(define (compute dfa state str)
		(define (state-next trans curr-state char)
			(cond
				((null? trans)
					(error "transition doesn't exist for state and character" curr-state char))
				((and (equal? (trans-state (car trans)) curr-state)
				      (equal? (trans-char  (car trans)) char))
					(trans-next (car trans)))
				(else (state-next (cdr trans) curr-state char))))
		(if (null? str)
			(is-element? state (dfa-finish dfa)) ; returns #t/#f if state is accept state.
			(compute dfa
			         (state-next (dfa-transition dfa) state (car str))
			         (cdr str))))
	(compute automata (dfa-start automata) string))
