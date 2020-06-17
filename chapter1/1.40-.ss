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
(define dx 0.00001)
(define (fixed-point f first-guess)
    (define (close-enough? a b)
        (> tolerance (abs (- a b))))
    (let ((next-guess (f first-guess)))
        (if (close-enough? next-guess first-guess)
            next-guess
            (fixed-point f next-guess))))
(define (average-damp f)
    (define (average a b) (/ (+ a b) 2))
    (lambda (x) (average x (f x))))
(define (fixed-point-normal g transform guess);求g经过trans之后的不动点
    (fixed-point (transform g) guess))
(define (deriv g);导数
    (lambda (x) 
        (/ (- (g (+ x dx)) (g x)) 
            dx)))

(define (newton-mothed g guess);牛顿法.计算g的零点
    (define (newton-trans g)
        (lambda (x)
            (- x (/ (g x) ((deriv g) x)))))
    (fixed-point (newton-trans g) guess))
(define (newton-sqrt x);牛顿法开根号
    (newton-mothed (lambda (y) (- (* y y) x)) 1.0))
;1.40
(define (cubic a b c)
    (lambda (x)
        (+ (* x x x)
           (* a x x)
           (* b x)
           c)))
;1.41
(define (double f)
    (lambda (x)
        (f (f x))))
(define (inc x) (+ x 1))
;1.42
(define (compose f g)
    (lambda (x)
        (f (g x))))
(define (square x) (* x x))
(define (inc x) (+ x 1))
;1.43 not perfect
(define (repeat f n)
    (define (involve n x)
        (if (= n 1) (f x)
            (f (involve (- n 1) x))))
    (lambda (x) (involve n x)))
;1.44
(define (smooth f)
    (lambda (x)
            (/ (+ (f x)
                  (f (+ x dx))
                  (f (- x dx)))
                3)))
; (define (smooth f x)
;     (/ (+ (f x)
;           (f (+ x dx))
;           (f (- x dx)))
;         3))
(define (repeat-smooth n)
    (repeat smooth n))
;1.45 有兴趣再写

(display
    (((repeat-smooth 2) square) 5))
(exit)