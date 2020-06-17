(define (find-divisor n cur)
    (cond ((> (* cur cur) n) n)
          ((divides? cur n) cur)
          (else (find-divisor n (+ cur 1)))))

(define (divides? a b)
    (= (remainder b a) 0))

(define (prime? n)
    (= (find-divisor n 2) n))
;费马检查
(define (square x) (* x x))

(define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
           (remainder (square (expmod base (/ exp 2) m))
                      m))
          (else 
           (remainder (* base (expmod base (- exp 1) m))
                m))))

(define (format-test n)
    (define (try-it a)
        (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
    (cond ((= times 0) true)
          ((format-test n) (fast-prime? n (- times 1)))
          (else false)))

(define (timed-prime-test n)
    (display n)
    (start-time-test n (runtime)))

(define (start-time-test n start-time)
    (if (prime? n)
        (report-prime (- (runtime) start-time))))

(define (report-prime totletime)
    (display " *** ")
    (display totletime))
