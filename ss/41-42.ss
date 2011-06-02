;#lang planet neil/sicp
(require (planet neil/sicp))

(begin

  (define the-empty-stream 'the-empty-stream)
  (define (stream-null? s) (eq? s the-empty-stream))

  (define (sum-primes a b)
    (define (iter count accum)
      (cond ((> count b) accum)
            ((prime? count) (iter (+ count 1) (+ count accum)))
            (else (iter (+ count 1) accum))))
    (iter a 0))

  (define (stream-enumerate-interval low high)
    (if (> low high)
        the-empty-stream
        (cons
         low
         (delay (stream-enumerate-interval (+ low 1) high)))))

  (define (stream-filter pred stream)
    (cond ((stream-null? stream) the-empty-stream)
          ((pred (stream-car stream))
           (cons (stream-car stream)
                 (delay (stream-filter pred
                                        (stream-cdr stream)))))
          (else (stream-filter pred (stream-cdr stream)))))

  (define (stream-ref s n)
    (if (= n 0)
        (stream-car s)
        (stream-ref (stream-cdr s) (- n 1))))
  (define (stream-map proc s)
    (if (stream-null? s)
        the-empty-stream
        (cons (proc (stream-car s))
              (delay (stream-map proc (stream-cdr s))))))
  (define (stream-for-each proc s)
    (if (stream-null? s)
        'done
        (begin (proc (stream-car s))
               (stream-for-each proc (stream-cdr s)))))

  (define (display-stream s)
    (stream-for-each display-line s))

  (define (display-line x)
    (newline)
    (display x))

  (define (stream-car stream) (car stream))
  (define (stream-cdr stream)
    (force  (cdr stream)))
  (define (cons-stream a b)
    (cons a (delay b))))
;; ***Streams ends here***


(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge
                          (scale-stream S 2)
                          (merge 
                           (scale-stream S 3)
                           (scale-stream S 5)))))




(define integers (cons-stream 1 (add-streams ones integers)))

(define (is-hamming? x)
  (cond ((= x 1) #t)
        ((= 0 (remainder x 2)) (is-hamming? (/ x 2)))
        ((= 0 (remainder x 3)) (is-hamming? (/ x 3)))
        ((= 0 (remainder x 5)) (is-hamming? (/ x 5)))
        (else #f)))

(define (S stream-filter is-hamming? integers))
