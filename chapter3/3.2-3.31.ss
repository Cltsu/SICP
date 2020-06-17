;3.12
;(b)
;(b c d)
(define (last-pair x)
    (if (null? (cdr x))
        x
        (last-pair (cdr x))))
;3.13
;环
;死循环
;3.14
(define (mystery x)
    (define (loop x y)
        (if (null? x) y
            (let ((temp (cdr x)))
                (set-cdr! x y)
                (loop temp x))))
    (loop x '()))
;reverse
;3.15 脑子里画
;3.16
(define (count-pairs x)
    (if (not (pair? x))
        0
        (+ (count-pairs (car x))
           (count-pairs (cdr x))
           1)))
;对于无环图是正确的
;3.17
(define (memq? x l)
    (cond ((null? l) #f)
          ((eq? x (car l)) #t)
          (else (memq? x (cdr l)))))
(define (right-count-pairs x)
    (define (iter x ret)
        (if (or (not (pair? x))
                 (memq? x ret))
            ret
            (iter (car x)
                  (iter (cdr x)
                         (cons x ret)))))
    (length (iter x '())))
;3.18
(define (cycle? l)
    (define (iter l visit)
        (cond ((null? l) #f)
              ((memq? l visit) #t)
              (else (iter (cdr l) (cons l visit)))))
    (iter l '()))
;3.19 做不来
;3.20

(define b (cons 1 (cons 2 '())))
(define c (cons b (cdr b)))
(define a (cons 1 '()))
(define (show . s)
    (map (lambda (x) (begin (display x) (newline) x)) s))
(display (cycle? (begin (set-cdr! a a) a)))
(exit)