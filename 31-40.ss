
;; 31

(define count 0)

(define (f x)
  (set! count (* (+ count x) x))
  count)

(+ (f 1) (f 2)) ;; 7
(+ (f 2) (f 1)) ;; 9

;;

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z
(a b c d)
(cdr x)
<response>
(define w (mappend x y))
w
(a b c d)
(cdr x)
<response>
