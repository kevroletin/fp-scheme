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

;; 24

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))
