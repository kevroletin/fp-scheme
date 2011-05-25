(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(accumulate + 0 (list 1 2 3 4 5))
;15
(accumulate * 1 (list 1 2 3 4 5))
;120
(accumulate cons '() (list 1 2 3 4 5))
;(1 2 3 4 5)

;; задание 18

(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))
(my-map (lambda (x) (* x 2)) '(1 2 3))

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))
(my-append '(1 2 3) '(4 5 6))

(define (my-length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
(my-length '(1 2 3))

;; 19

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

;; 20

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(let ((a '(1 2 3 4)))
  (dot-product a a))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(let ((v '(1 2 3 4))
      (m '((1 1 1 1)
           (2 2 2 2)
           (3 3 3 3))))
  (matrix-*-vector m v))

(define (transpose mat)
  (accumulate-n (lambda (x y) (cons x y)) '() mat))

(transpose '((1 2) (3 4)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector n row) ) m)))

(let ((m '((1 2 3)
          (3 4 5)
          (6 7 8)))
      (n '((1 1 1)
           (1 1 1)
           (1 1 1))))
  (matrix-*-matrix m n))

;; 21

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list '() (list 1 2 3))
(fold-left list '() (list 1 2 3))

;; 22

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(reverse '(1 2 3))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

;; 23

(define (equal? a b)
  (cond
   ((and (pair? a) (pair? b))
    (and (eq? (car a) (car b)) (equal? (cdr a) (cdr b))))
   ((and (not (pair? a)) (not (pair? b)))
    (eq? a b))
   (else '())))

(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))
(equal? '(1 2) '(1 2)) ; > #t
(equal? '(1 2) '(2 1)) ; > #f
(equal? '(1 2 3) '(1 2)) ; > #f

;; 24

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (cons '+
               (foldr (lambda (x y) (cons (deriv x var) y))
                      '()  (sum-args exp))))
        ((product? exp)
         (make-sum
           (make-product (deriv (product-first-arg exp) var)
                         (product-last-args exp))
           (make-product (product-first-arg exp)
                         (deriv (product-last-args exp) var))))
        ((power? exp)
         (make-product
           (make-power (power-get-arg exp) (- (power-get-pow exp) 1))
           (deriv (power-get-arg exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 . a2) (append (list '+ a1) a2))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (sum-args s) (cdr s))
(define (sum-first-arg s) (car (sum-args s)))
(define (sum-last-args s) (cdr (sum-args s)))

(define s (make-sum 1 2 3))
(sum-args s) ;; > (1 2 3)


(define (make-product m1 . m2) (append (list '* m1) m2))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (product-args p) (cdr p))
(define (product-first-arg s) (car (product-args s)))
(define (product-last-args s)
  (let ((tail (cdr (product-args s))))
    (if (>= (length tail) 2)
        (cons '* tail)
        (car tail))))

(define p (make-product 1 2 3))
(product-args p)                          ;; > (1 2 3)
(product-first-arg p)                     ;; > 1
(product-last-args p)                     ;; > (* 2 3)
(product-last-args (product-last-args p)) ;; >3

(define (make-power num pow) (list '^ num pow))

(define (power? x)
  (and (pair? x) (eq? (car x) '^)))

(define (power-get-arg p) (cadr p))
(define  (power-get-pow p)  (caddr p))

(deriv (make-sum 'x 'x 1) 'x)   ;; > (+ 1 1 0)
(deriv (make-product 'x 'x) 'x) ;; > (+ (* 1 x) (* x 1))

(define (check-for-oper x op)
  (and
   (not (null? x))
   (not (null? (cdr x)))
   (eq? (cadr x) op)))

(define (mult? x)
  (check-for-oper x '*))

(define (sum? x)
  (check-for-oper x '+))

(define (select-mult p)
  (define (iter l res)
    (if (not (mult? l)) (cons (car l) res)
        (iter (cddr l) (cons '* (cons (car l) res)))))
  (iter p '()))

(define (select-after-mult p)
  (define (iter l res)
    (if (not (mult? l)) (cddr p)
        (iter (cddr l) (cons '* (cons (car l) res)))))
  (iter p '()))

(define (variable? x)
  (and (not (null? x))
       (null? (cdr x))
       (symbol? (car x))))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum l1 l2)
  (append l1 (list '+) l2))

(define (make-mult l1 l2)
  (append l1 (list '*) l2))

(mult? '(1 * 2 * 3 + 5))
(select-mult '(1 * 2 * 3)) 

(cadr '(1 2 3))
(cddr '(1 2 3))
(cadr '(1 2))
(variable? (list 'x))
