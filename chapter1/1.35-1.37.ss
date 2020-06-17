(define (f-lambda x y)
    ((lambda (a b)
              (+ (* x (* a a))
                 (* y b)
                 (* a b)))
        (+ 1 (* x y))
        (- 1 y)))
;定义let变量所使用的变量由外部提供
(define (f-let x y)
    (let ((a (+ 1 (* x y)))
          (b (- 1 y)))
    (+ (* x (* a a))
       (* y b)
       (* a b))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
    (define (close-enough? a b)
        (> tolerance (abs (- a b))))
    (let ((next-guess (f first-guess)))
        (if (close-enough? next-guess first-guess)
            next-guess
            (fixed-point f next-guess))))
;1.35
; (fixed-point (lambda (x) (+ 1 (/ 1 x))) 2.0))
;1.36
;1.37
(define (infinite k)
    (if (= k 1) 1.0
        (/ 1.0 (+ 1 (infinite (- k 1))))))
(define (infinite-iter k)
    (define (iter k cur)
        (if (= k 1) cur
            (iter (- k 1) (/ 1 (+ 1 cur)))))
    (iter k 1.0))
