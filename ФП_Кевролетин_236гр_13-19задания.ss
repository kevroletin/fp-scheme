;; 14 last-pair

(define (last-pair list)
  (if (null? (cdr list)) (car list)
      (last-pair (cdr list))))

(last-pair '(1 2 3)) ; 3
(last-pair '(1))     ; 1
(last-pair '())      ; error

;; 15 reverse

(define (reverse-rec list res)
  (if (null? list) res
      (reverse-rec (cdr list) (cons (car list) res))))

(define (reverse list)
  (reverse-rec list '()))

(my-reverse '(1 2 3)) ; (3 2 1)
(my-reverse '())      ; ()

;; 16 same-parity

(define (xor a b) (or (and a (not b)) (and b (not a))))
(define (equiv a b) (not (xor a b)))

(define (filter predicate list)
  (if (null? list) '()
      (if (predicate (car list))
          (cons (car list) (filter predicate (cdr list)))
          (filter predicate (cdr list)))))

(define (same-parity list)
  (filter
   (lambda (a) (equiv (even? (car list)) (even? a)))
   list))

(my-filter odd? '(1 2 3)) ; (1 3)
(my-filter odd? '())      ; ()

(same-parity '(1 2 3))     ; (1 3)
(same-parity '(2 3 4 5 6)) ; (2 4 6)
(same-parity '())          ; ()

;; 17 deep-reverse

(define (deep-reverse list)
  (reverse 
   (map
    (lambda (elem)
      (if (pair? elem) (deep-reverse elem) elem))
    list)))

(deep-reverse '(1 2 3))               ; (3 2 1)
(deep-reverse '(1))                   ; (1)
(deep-reverse '())                    ; ()
(deep-reverse '(1 2 3 (1 2 3)))       ; ((3 2 1) 3 2 1)
(deep-reverse '(1 2 (4 5 (6 7 8)) 3)) ; (3 ((8 7 6) 5 4) 2 1)

;; 18 fringe

(define (append list1 list2)
  (if (null? list1) list2 
      (cons (car list1) (my-append (cdr list1) list2))))

(append '(1 2) '(3 4)) ; (1 2 3 4)
(append '() '(3 4))    ; (3 4)
(append '(1 2) '())    ; (1 2)

(define (fringe data)
  (if (not (pair? data)) (if (null? data) '()  (list data))
      (foldr
       (lambda (elem res) (append (fringe elem) res))
       '()
       data)))

(fringe '(1 2 3)) ; (1 2 3)
(fringe '(1))     ; (1)
(fringe '())      ; ()

(fringe '(1 (2 2) 3 ()))              ; (1 2 2 3)
(fringe '(1 (2 2) (3 (4 (5) 6)) ()))  ; (1 2 2 3 4 5 6)

;; 19 tree-map

(define (tree-map funct tree)
  (if (not (pair? tree)) (funct tree)
      (map (lambda (elem) (tree-map funct elem))
           tree)))

(tree-map (lambda (x) (+ x 1)) '(1 2 3)) ; (2 3 4)
(tree-map (lambda (x) (+ x 1)) '(1))     ; (2)
(tree-map (lambda (x) 1) '())            ; 1

(tree-map (lambda (x) (+ x 1)) '(1 (2 (3) 4) 5 (6))) ; (2 (3 (4) 5) 6 (7))
