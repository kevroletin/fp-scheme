
;; 31

(define count 0)

(define (f x)
  (set! count (* (+ count x) x))
  count)

(+ (f 1) (f 2)) ;; 7
(+ (f 2) (f 1)) ;; 9

;;

(define a '(1 2 3))

(define (in-list l x)
  (cond ((null? l) 0)
        ((eq? (car l) x) 1)
        (else (in-list (cdr l) x))))

(define (push l x)
  (set-cdr! l (pair x '())))

(define counted '())

(define (count-pairs x)
  (cond
   ((not (pair? x)) 0)
   ((in-list counted x) 0)
   (else
    (push counted x)
    (+ (count-pairs (car x))
            (count-pairs (cdr x))
            1))))


;;

(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          ((eq? m 'print)
           (if (null? y) (list  x '())
               (list x (print y))))
          (else (error "Undefined operation -- CONS" m))))
  dispatch)

(define (print z) (z 'print))
(define (car z) (z 'car))
(define (cdr z) (z 'cdr))
(define (set-car! z new-value)
  ((z 'set-car!) new-value)
  z)
(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;;

(define (print-queue q) (pront-ptr q))


(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))

    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue")
      (car front-ptr)))
    (define (insert-queue!)
      (lambda (item)
        (let ((new-pair (cons item '())))
          (cond ((empty-queue?)
                 (set! front-ptr new-pair)
                 (set! rear-ptr new-pair))
                (else
                 (set-cdr! rear-ptr new-pair)
                 (set! rear-ptr new-pair))))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue"))
            (else
             (set! front-ptr (cdr front-ptr))
             queue)))

    (define (dispatch m)
      (cond
       ((eq? m 'front-ptr) front-ptr)
       ((eq? m 'rear-ptr) rear-ptr)
       ((eq? m 'empty-queue?) (empty-queue?))
       ((eq? m 'front-queue) (front-queue))
       ((eq? m 'insert-queue!) (insert-queue!))
       ((eq? m 'delete-queue!) (delete-queue!))
       (else (error "Undefined operation -- QUEUE" m))))
    dispatch))

(define (front-ptr q) (q 'rear-ptr))
(define (rear-ptr q) (q 'front-ptr))
(define (empty-queue? q) (q 'empty-queue?))
(define (front-queue q) (q 'front-queue))
(define (insert-queue! q v) ((q 'insert-queue!) v))
(define (delete-queue! q) (q 'delete-queue!))
