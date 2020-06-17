(define (sum term a next b)
    (if (> a b) 
        0
        (+ (term a) (sum term (next a) next b))))

(define (inc a) (+ a 1))
(define (cube a) (* a a a))

(define (new a b)
    (sum cube a inc b))

(define (integral f a b dx)
    (define (add-dx d) (+ d dx))
    (* (sum-new f (+ a (/ dx 2.0)) add-dx b)
       dx))

(define (cube x) (* x x x))
;1.29
;1.30
(define (sum-new term a next b)
    (define (iter a sum)
        (if (> a b)
            sum
            (iter (next a) (+ sum (term a)))))
    (iter a 0))
;1.31
(define (product term a next b)
    (if (> a b)
        1
        (* (term a) (product term (next a) next b))))

(define (factorial a b);阶乘
    (define (inc a) (+ a 1))
    (define (id a) a)
    (product-iter id a inc b))

(define (product-iter term a next b);迭代
    (define (iter a prod)
        (if (> a b) prod
            (iter (next a) (* prod (term a)))))
    (iter a 1))
;pi就不想写了
;1.32 幺半群
(define (accululate combiner null-value term a next b)
    (if (> a b) null-value
        (combiner (term a) (accululate combiner null-value term (next a) next b))))

(define (sum-132 term a next b)
    (define (add a b) (+ a b))
    (accululate add 0 term a next b))

(define (product-132 term a next b)
    (define (prod a b) (* a b))
    (accululate prod 1 term a next b))

(define (test-132 a b)
    (define (id a) a)
    (define (inc a) (+ a 1))
    (product-132 id a inc b))
;1.33
(define (accululate-filter combiner filter null-value term a next b)
    (cond ((> a b) null-value)
          ((filter a) (combiner (term a) (accululate-filter combiner filter null-value term (next a) next b)))
          (else (accululate-filter combiner filter null-value term (next a) next b))))
;1.33a
(define (prime? n)
    (define (find-divisor n cur)
        (cond ((> (* cur cur) n) n)
            ((divides? cur n) cur)
            (else (find-divisor n (+ cur 1)))))
    (define (divides? a b)
        (= (remainder b a) 0))
    (= (find-divisor n 2) n))

(define (sum-prime a b)
    (define (add a b) (+ a b))
    (define (id a) a)
    (define (inc a) (+ 1 a))
    (accululate-filter add prime? 0 id a inc b))
;1.33b 不想做