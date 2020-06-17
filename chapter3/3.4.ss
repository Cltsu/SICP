;3.39
;101 121 100 11
;3.40
;10^2 10^3 10^4  10^5 10^6
;10^6
;3.41
;不同意，不修改值的操作是可以不加锁的
;3.42
;这道题应该需要只是protect具体的实现方式
;3.43
;3.44
;转移问题是余额无关的，只要操作被执行了，顺序是怎样的都不会影响结果。
;3.45
;显然Louis是个麻瓜，只有放弃存款和取款的自动串行化才能将锁取出来放在更大的抽象层面。
;3.46
;3.47
(define (make-mutex)
    (let ((cell (list #f)))
        (define (the-mutex m)
            (cond ((eq? m 'acquire)
                   (if (test-and-set! cell)
                       (the-mutex 'acquire)))
                  ((eq? m 'release) (clear! cell))))
        the-mutex))
(define (clear! cell)
    (set-car! cell #f))
(define (test-and-set! cell)
    (if (car cell)
        #t
        (begin (set-car! cell #t)
               #f)))
;a)
(define (make-semaphore n)
    (let ((mutex (make-mutex)))
        (define (acquire)
            (mutex 'acquire)
            (if (= n 0)
                (begin (mutex 'release)
                       (acquire))
                (begin (set! n (- n 1))
                       (mutex 'release))))
        (define (release)
            (mutex 'acquire)
            (set! n (+ n 1))
            (mutex 'release))
        (define (dispatch m)
            (cond ((eq? m 'acquire) (acquire))
                  ((eq? m 'release) (release))
                  (else (error "valid op" m))))
        dispatch))
;b)
(define (make-semaphore2 n)
    (define (semaphore m)
        (cond ((eq? m 'acqiure)
               (if (test-and-set! n)
                   (semaphore m)))
              ((eq? m 'release)
               (set! n (+ n 1)))))
    semaphore)
(define (test-and-set! n)
    (if (> n 0)
        (begin (set! n (- n 1))
               #f)
        #t))
;
(define (show . s)
    (map (lambda (x) (begin (display x) (newline) x)) s))
(display (test 'show))
(exit)