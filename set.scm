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

(define (element-of-set? x set)
	(cond
		((null?  set) #f)
		((equal? x (car set)) #t)
		(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
	(if (element-of-set? x set)
	    set
	    (cons x set)))

(define (intersection-set set1 set2)
	(cond
		((or (null? set1) (null? set2)) '())
		((element-of-set? (car set1) set2)
			(cons (car set1) (intersection-set (cdr set1) set2)))
		(else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
	(cond
		((null? set1) set2)
		((element-of-set? (car set1) set2)
			(union-set (cdr set1) set2))
		(else (union-set (cdr set1) (cons (car set1) set2)))))
