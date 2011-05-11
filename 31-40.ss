
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
             (set! front-ptr (cdr front-ptr)))))

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


;; dequeue

;#lang planet neil/sicp
(require (planet neil/sicp))

(define (make-dequeue)
  (let ((front-ptr '())
        (back-ptr '()))

    (define (empty-dequeue?) (null? front-ptr))
    (define (front-dequeue)
      (if (empty-dequeue?)
          (error "FRONT called with an empty dequeue")
          (car (car front-ptr))))
    (define (back-dequeue)
      (if (empty-dequeue?)
          (error "FRONT called with an empty dequeue")
          (car (car back-ptr))))
    (define (insert-front-dequeue!)
      (lambda (item)
        (let ((new-bottom-pair (cons '() '())))
          (let ((new-top-pair (cons item new-bottom-pair)))
            (cond ((empty-dequeue?)
                   (set! front-ptr new-top-pair)
                   (set! back-ptr new-top-pair))
                  (else
                   (set-car! (cdr front-ptr) new-top-pair)
                   (set-cdr! new-bottom-pair front-ptr)
                   (set! front-ptr new-top-pair)))))))
    (define (insert-back-dequeue!)
      (lambda (item)
        (let ((new-bottom-pair (cons '() '())))
          (let ((new-top-pair (cons item new-bottom-pair)))
            (cond ((empty-dequeue?)
                   (set! front-ptr new-top-pair)
                   (set! back-ptr new-top-pair))
                  (else
                   (set-cdr! (cdr back-ptr) new-top-pair)
                   (set-car! new-bottom-pair back-ptr)
                   (set! back-ptr new-top-pair)))))))
    (define (delete-front-dequeue!)
      (cond ((empty-dequeue?)
             (error "DELETE! called with an empty queue"))
            (else
             (set! front-ptr (cdr (cdr front-ptr)))
             (if (not (null? front-ptr))
                 (set-car! (cdr front-ptr) '())))))
    (define (delete-back-dequeue!)
      (cond ((empty-dequeue?)
             (error "DELETE! called with an empty queue"))
            (else
             (set! back-ptr (car (cdr back-ptr)))
             (if (not (null? back-ptr))
                 (set-cdr! (cdr back-ptr) '())))))
    (define (print-dequeue back-node result)
      (if (null? back-node) '(result)
          (let ((new-res (cons (car back-node) result)))
            (print-dequeue (car (cdr back-node)) new-res))))
    
    (define (dispatch m)
      (cond
       ((eq? m 'front-ptr) front-ptr)
       ((eq? m 'back-ptr) back-ptr)
       ((eq? m 'empty-dequeue?) (empty-dequeue?))
       ((eq? m 'front-dequeue) (front-dequeue))
       ((eq? m 'back-dequeue) (back-dequeue))
       ((eq? m 'insert-front-dequeue!) (insert-front-dequeue!))
       ((eq? m 'insert-back-dequeue!) (insert-back-dequeue!))
       ((eq? m 'delete-front-dequeue!) (delete-front-dequeue!))
       ((eq? m 'delete-back-dequeue!) (delete-back-dequeue!))
       ((eq? m 'print-dequeue) (print-dequeue back-ptr))
       (else (error "Undefined operation -- QUEUE" m))))
    dispatch))

(define (front-ptr q) (q 'front-ptr))
(define (back-ptr q) (q 'back-ptr))
(define (empty-dequeue? q) (q 'empty-dequeue?))
(define (front-dequeue q) (q 'front-dequeue))
(define (insert-front-dequeue! q v) ((q 'insert-front-dequeue!) v))
(define (insert-back-dequeue! q v) ((q 'insert-back-dequeue!) v))
(define (delete-front-dequeue! q) (q 'delete-front-dequeue!))
(define (delete-back-dequeue! q) (q 'delete-back-dequeue!))
(define (print-dequeue q) (q 'print-dequeue))
