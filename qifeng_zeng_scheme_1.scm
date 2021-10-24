;;;Problem 1
;;;(byTwos n m) returns the list of every other integer starting with n up to m.
;;;Base case: if n>m return an empty list.
;;;Hypothesis: Assume (byTwos (+ n 2) m) returns the list of every
;;;other integer from n+2 up to m.
;;;Recursive step: (byTwos n m) returns (cons n (byTwos(+ n 2) m)).
(define (byTwos n m)
  (cond((> n m)'())
       (else (cons n (byTwos(+ n 2) m)))))
;;;Problem 2
;;;(compress L) returns a list of all atoms (non-list values) contained in L or in any nested list within L.
;;;Base case: if L is empty then return an empty list.
;;;Hypothesis: Assume (compress (cdr L)) returns a list of all atoms (non-list values) contained in cdr L or in any nested list within cdr L.
;;;Recursive step: if (car L) is a list, (compress L) returns (append (compress (car L)) (compress (cdr L))) else returns (cons (car L) (compress (cdr L))).
(define (compress L)
  (cond((null? L) '())
       (else (if (list? (car L))
                 (append (compress (car L)) (compress (cdr L)))
                 (cons (car L) (compress (cdr L)))))))
;;;Problem 3
;;;Use helper function (myrev old new) to reverse the elements of a list old and put the answer in new.
;;;Base case: if old is empty then return new.
;;;Hypothesis: Assume (myrev (cdr old) new) returns the reversed (cdr old).
;;;Recursive step: if (car L) is a list, (myrev old new) returns (myrev (cdr old) (cons (myrev (car old) '()) new))) else returns (myrev (cdr old) (cons (car old) new))
(define (rev-all L) (myrev L '()))
(define (myrev old new)
  (cond((null? old) new)
       (else (if (list? (car old))
                 (myrev (cdr old) (cons (myrev (car old) '()) new))
                 (myrev (cdr old) (cons (car old) new))))))
;;;Problem 4
;;;(equalTo? x y) return whether 2 lists, x and y, are the same.
;;;Base case: If x is empty but y is not then #f, and it also returns #f vice versa. If x and y are both empty return #t.
;;;Hypothesis: Assume (equalTo? (cdr x) (cdr y)) returns whether (cdr x) and (cdr y) are the same.
;;;Recursive step: if (eq? (car x) (car y)), (equalTo? x y) returns (equalTo? (cdr x) (cdr y)), else returns #f.
(define (equalTo? x y)
  (if (or (null? x) (null? y))
      (cond ((and (null? x) (null? y)) '#t)
            (else '#f))
      (cond ((eq? (car x) (car y)) (equalTo? (cdr x) (cdr y)))
            (else '#f))))
;;;Problem 5
;;;(equalFns? fn1 fn2 domain) return whether fn1 and fn2 return the same value when applied to the same element of domain.
;;;Base case: If domain is empty, then return #t.
;;;Hypothesis: Assume (equalFns? fn1 fn2 (cdr domain)) return whether fn1 and fn2 return the same value when applied to the same element of (cdr domain).
;;;Recursive step: if (equal? (fn1 (car domain)) (fn2 (car domain))), then (equalFns? fn1 fn2 domain) returns (equalFns? fn1 fn2 (cdr domain)), else return #f.
(define (equalFns? fn1 fn2 domain)
  (cond ((null? domain) #t)
        (else (if (equal? (fn1 (car domain)) (fn2 (car domain)))
                  (equalFns? fn1 fn2 (cdr domain))
                  #f))))
;;;Problem 6
;;;(same-vals fn1 fn2 domain) returns the list of all elements x of domain such that (fn1 x) and (fn2 x) return the same value.
;;;use (mysame-vals fn1 fn2 domain res) to help.
;;;Base case: If domain is empty, then return res.
;;;Hypothesis: Assume (mysame-vals fn1 fn2 (cdr domain) res) gives correct result.
;;;Recursive step: if (equal? (fn1 (car domain)) (fn2 (car domain))), then (mysame-vals fn1 fn2 (cdr domain) (cons (car domain) res))
;;;else returns (mysame-vals fn1 fn2 (cdr domain) res)
(define (same-vals fn1 fn2 domain) (mysame-vals fn1 fn2 domain '()))
(define (mysame-vals fn1 fn2 domain res)
  (cond ((null? domain) res)
        (else (if (equal? (fn1 (car domain)) (fn2 (car domain)))
                  (mysame-vals fn1 fn2 (cdr domain) (cons (car domain) res))
                  (mysame-vals fn1 fn2 (cdr domain) res)))))
;;;Problem 7
;;;(split x L) returns a list containing two lists: The first contains the numbers in L less than or equal to x
;;;and the second contains the numbers in L greater than x.
;;;Base case: If L is empty, then return '(()()).
;;;Hypothesis: Assume (split x (cdr L)) gives the correct result.
;;;Recursive step: if (>(car L) x), then returns (append (car (split x (cdr L))) (cons (car L) (cadr (split x (cdr L)))))
;;;else returns (append (cons (car L) (car (split x (cdr L)))) (cadr (split x (cdr L)))).
(define (split x L)
  (cond ((null? L) '(()()))
        (else (if (>(car L) x)
                  (cons (car (split x (cdr L))) (cons (cons (car L) (cadr (split x (cdr L)))) '()))
                  (cons (cons (car L) (car (split x (cdr L)))) (cons (cadr (split x (cdr L))) '()))))))
;;;Problem 8
;;;(psort L) implements a partition sort.
;;;Base case: If L is empty, then return '().
;;;Hypothesis: (psort L') works on any L' that is shorter than L.
;;;Recursive step: (psort L) returns (list (psort (car (split (car L) (cdr L))) (car L) (psort (cadr (split (car L) (cdr L)))))
(define (psort L)
  (cond ((null? L) '())
        (else (append (psort (car (split (car L) (cdr L)))) (cons (car L) (psort (cadr (split (car L) (cdr L)))))))))
;;;Problem 9
;;;(applyToList f) returns a function that takes a list L as a parameter and applies f to every element of L, returning the resulting list as the result.
(define (applyToList f)
  (lambda (L) (map f L)))
;;;Problem 10
;;;Create my own map function in newApplyToList, write a recursive mymap function inside newApplyToList function.
(define (newApplyToList f)
  (lambda (L)
    (cond ((null? L) '())
        (else (cons (f (car L))  (newApplyToList f (cdr L)))))))