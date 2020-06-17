(define (memo-proc proc)
    (let ((already-run? #f) (result #f))
        (lambda ()
            (if (not already-run?)
                (begin (set! result (proc))
                       (set! already-run? #t)
                       result)
                result))))
;delay和cons-stream都必须用macro实现
; (define (delay proc)
;     (memo-proc (lambda () proc))) 
; (define (dalay proc)
;     (lambda () proc))
(define (force delay-proc)
    (delay-proc))
(define-syntax delay
  (syntax-rules ()
    ((_ exp) (memo-proc (lambda () exp)))))
(define-syntax cons-stream
  (syntax-rules ()
    [(cons-stream x y) (cons x (delay y))]))
(define (stream-car a) (car a))
(define (stream-cdr a) (force (cdr a)))
(define the-empty-stream '())
(define (stream-null? s) (null? s))
(define (stream-enumerate-interval low high)
    (if (> low high)
        the-empty-stream
        (cons-stream
            low
            (stream-enumerate-interval (+ low 1) high))))
(define (stream-map proc . args)
    (cond ((null? args) the-empty-stream)
          ((stream-null? (car args)) the-empty-stream)
          (else (cons-stream (apply proc (map stream-car args))
                             (apply stream-map
                                    (cons proc
                                          (map stream-cdr args)))))))
(define (stream-ref s n)
    (if (= n 0)
        (stream-car s)
        (stream-ref (stream-cdr s) (- n 1))))
(define (stream-filter pred stream)
    (cond ((stream-null? stream) the-emtpy-stream)
          ((pred (stream-car stream))
           (cons-stream (stream-car stream)
                        (stream-filter pred (stream-cdr stream))))
          (else (stream-filter pred (stream-cdr stream)))))
;
(define (integers-starting-from n)
    (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))
(define (divisible? x y) (= (remainder x y) 0))
(define (sieve stream)
    (cons-stream
     (stream-car stream)
     (sieve (stream-filter
             (lambda (x)
                (not (divisible? x (stream-car stream))))
             (stream-cdr stream)))))
(define primes (sieve (integers-starting-from 2)))
(define (add-streams s1 s2)
    (stream-map + s1 s2))
(define (scale-stream s n)
    (stream-map (lambda (x) (* x n)) s))
;3.53
;2的幂
;3.54
(define (mul-streams s1 s2)
    (cons-stream (* (stream-car s1)
                    (stream-car s2))
                 (mul-streams (stream-cdr s1)
                             (stream-cdr s2))))
(define factorials (cons-stream 1 (mul-streams integers factorials)))
;3.55
(define (partial-sums s)
    (cons-stream (stream-car s)
                 (add-streams (partial-sums s)
                              (stream-cdr s))))
;3.56
(define (merge s1 s2)
    (cond ((stream-null? s1) s2)
          ((stream-null? s2) s1)
          (else
            (let ((s1car (stream-car s1))
                  (s2car (stream-car s2)))
                (cond ((< s1car s2car)
                       (cons-stream s1car (merge (stream-cdr s1) s2)))
                      ((> s1car s2car)
                       (cons-stream s2car (merge s1 (stream-cdr s1))))
                      (else
                        (cons-stream s1car
                                     (merge (stream-cdr s1)
                                            (stream-cdr s2)))))))))
(define S (cons-stream 1 (merge (scale-stream S 2)
                                (merge (scale-stream S 3)
                                       (scale-stream S 5)))))
;3.57
;确实
;3.58
(define (expand num den radix)
    (cons-stream
        (quotient (* num radix) den)
        (expand (remainder (* num radix) den) den radix)))
;3.59
(define ones (cons-stream 1 ones))
(define (div-stream s1 s2)
    (cons-stream (/ (stream-car s1) (stream-car s2))
                 (div-stream (stream-cdr s1) (stream-cdr s2))))
(define (integrate-series s)
    (mul-streams s (div-stream ones integers)))
;b)
;3.60 级数搞忘完了
;3.61
;3.62
;
;
;
(define (average x y)
    (/ (+ x y) 2.0))
(define (sqrt-improve guess x)
    (average guess (/ x guess)))
(define (sqrt-stream x)
    (define guesses
        (cons-stream 1.0
                     (stream-map (lambda (guess) (sqrt-improve guess x))
                                 guesses)))
    guesses)

(define (pi-summands n)
    (cons-stream (/ 1.0 n)
                 (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
    (scale-stream (partial-sums (pi-summands 1)) 4))
(define (square x) (* x x))
(define (euler-transform s)
    (let ((s0 (stream-ref s 0))
          (s1 (stream-ref s 1))
          (s2 (stream-ref s 2)))
        (cons-stream (- s2 (/ (square (- s2 s1))
                              (+ s0 (* -2 s1) s2)))
                     (euler-transform (stream-cdr s)))))
(define (make-tableau transform s)
    (cons-stream s
                 (make-tableau transform
                               (transform s))))
(define (acclerated-sequence transform s)
    (stream-map stream-car
                (make-tableau transform s)))
;3.63
;louis的过程时间复杂度是O(n^2)，有guesses的版本则是O(n)
;没有memo-proc会使带guesses的版本也变为O(n^2)复杂度
;3.64
(define (stream-limit s limit)
    (let ((front (stream-car s))
          (later (stream-car (stream-cdr s))))
        (if (> limit (abs (- front later)))
            later
            (stream-limit (stream-cdr s) limit))))
(define (sqrt x tolerance)
    (stream-limit (sqrt-stream x) tolerance))
;3.65
(define (ln2-seq n)
    (cons-stream (/ 1.0 n)
                 (stream-map - (ln2-seq (+ n 1)))))
(define ln2 (partial-sums (ln2-seq 1)))
;
;
;
(define (interleave s1 s2)
    (if (stream-null? s1)
        s2
        (cons-stream (stream-car s1)
                     (interleave s2 (stream-cdr s1)))))
(define (pairs s t)
    (cons-stream
        (list (stream-car s) (stream-car t))
        (interleave
            (stream-map (lambda (x) (list (stream-car s) x))
                        (stream-cdr t))
            (pairs (stream-cdr s) (stream-cdr t)))))
;3.66
;对于前后次序的序对p1(i1,j1)，p2(i2,j2)满足i2>=i1,如果i1=i2，则必有j2>j1
;3.67
(define (interleave-three s1 s2 s3)
    (if (stream-null? s1)
        s2
        (cons-stream (stream-car s1)
                     (interleave-three s2 s3 (stream-cdr s1)))))
(define (all-pairs s t)
    (cons-stream
        (list (stream-car s) (stream-car t))
        (interleave-three
            (stream-map (lambda (x) (list (stream-car s) x))
                        (stream-cdr t))
            (stream-map (lambda (x) (list x (stream-car t)))
                        (stream-cdr s))
            (all-pairs (stream-cdr s) (stream-cdr t)))))
;3.68
;构造出来的将不是stream而是list
(define (pairs-mod s t)
    (interleave
        (stream-map (lambda (x) (list (stream-car s) x))
                    t)
        (pairs-mod (stream-cdr s) (stream-cdr t))))
;3.69
(define (triples s t u)
    (cons-stream
        (list (stream-car s) (stream-car t) (stream-car u))
        (interleave
            (stream-map (lambda (x) (cons (stream-car s) x))
                (interleave
                    (stream-map (lambda (x) (list (stream-car t) x))
                                (stream-cdr u))
                    (pairs (stream-cdr t) (stream-cdr u))))
            (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))
;3.70
(define (merge-weighted s1 s2 weight)
    (cond ((stream-null? s1) s2)
          ((stream-null? s2) s1)
          ((> (weight (stream-car s2))
              (weight (stream-car s1)))
            (cons-stream (stream-car s1)
                         (merge-weighted s2 (stream-cdr s1) weight)))
          ((> (weight (stream-car s1))
              (weight (stream-car s2)))
            (cons-stream (stream-car s2)
                         (merge-weighted (stream-cdr s2) s1 weight)))
          (else (cons-stream (stream-car s1)
                             (merge-weighted (stream-cdr s1) s2 weight)))))
(define (pairs-weighted s t weight)
    (cons-stream
        (list (stream-car s) (stream-car t))
        (merge-weighted
            (stream-map (lambda (x) (list (stream-car s) x))
                        (stream-cdr t))
            (pairs-weighted (stream-cdr s) (stream-cdr t) weight)
            weight)))
;a)
(define (weight pair)
    (+ (car pair)
       (cadr pair)))
;b)
(define (weight-b pair)
    (let ((i (car pair))
          (j (cadr pair)))
        (+ (* 2 i)
           (* 3 j)
           (* 5 i j))))
(define (filter-b s)
    (define (pred p)
        (let ((i (car p)) (j (cadr p)))
            (or (or (= 0 (remainder i 2))
                    (= 0 (remainder i 3))
                    (= 0 (remainder i 5)))
                (or (= 0 (remainder j 2))
                    (= 0 (remainder j 3))
                    (= 0 (remainder j 5))))))
    (stream-filter pred s))
;3.71
;1729 4104 13832 20683 32832
(define (weight-ramanujan p)
    (let ((i (car p)) (j (cadr p)))
        (+ (* i i i)
           (* j j j))))
(define ramanujan
    (let ((s (pairs-weighted integers integers weight-ramanujan)))
        (define (filter-r s last)
            (let ((cur (stream-car s)))
                (if (= (weight-ramanujan last)
                       (weight-ramanujan cur))
                    (cons-stream (weight-ramanujan cur)
                                 (filter-r (stream-cdr s) cur))
                    (filter-r (stream-cdr s) cur))))
        (filter-r (stream-cdr s) (stream-car s))))
;3.72
(define (weight-square p)
    (let ((i (car p)) (j (cadr p)))
        (+ (* i i)
           (* j j))))
(define p372
    (let ((s (pairs-weighted integers integers weight-square)))
        (define (filter-372 s one two)
            (let ((three (stream-car s)))
                (if (= (weight-square one)
                       (weight-square two)
                       (weight-square three))
                    (cons-stream (list (weight-square one) one two three)
                                 (filter-372 (stream-cdr s) two three))
                    (filter-372 (stream-cdr s) two three))))
        (let ((one (stream-car s))
              (two (stream-car (stream-cdr s)))
              (rest (stream-cdr (stream-cdr s))))
            (filter-372 rest one two))))
;3.73 RC电路不会
;3.74 如果要考察流中的相邻对象，可以比较s和(stream-cdr s)
(define zero-crossings
    (stream-map sign-change-detector sense-data (stream-cdr sense-date)))
;3.75
(define (make-zero-crossings input-stream last-value last-avpt)
    (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
        (cons-stream (sign-change-detector avpt last-value)
                     (make-zero-crossings (stream-cdr input-stream) (stream-car input-stream) avpt))))
;3.76
(define (smooth s)
    (define (iter s last-value last-avpt)
        (let ((avpt (/ (+ (stream-car s) last-value) 2)))
            (cons-stream avpt
                         (iter (stream-cdr s) (steam-car s) avpt))))
    (iter (stream-cdr s) (stream-car s) (stream-car s)))
(define zero-crossings376
    (let ((s (smooth sense-date)))
        (stream-map sign-change-detector s (steam-cdr s))))
;
;
;
;
(define (show-stream s)
    (if (stream-null? s) '()
        (begin (display (stream-car s))
               (show-stream (stream-cdr s)))))
(define (show-stream-n s n)
    (if (= n 0) '()
        (begin (display (stream-car s))
               (newline)
               (show-stream-n (stream-cdr s) (- n 1)))))
(define s5 (stream-enumerate-interval 1 10))
; (show-stream (stream-map-formal + s4 s5))
(show-stream-n p372 5)
(exit)