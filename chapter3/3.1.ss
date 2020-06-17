(define random-init 0)
(define rand
    (let ((x random-init))
        (lambda ()
            (set! x (rand-update x))
            x)))
;3.1
(define (make-accumulator init)
    (lambda (num)
            (begin (set! init (+ init num))
                   init)))
(define A (make-accumulator 0))
;3.2
(define (make-monitored f)
    (define count 0)
    (define (dispatch m)
        (cond ((eq? m 'how-many-times) count)
              ((eq? m 'reset-count) (set! count 0))
              (else (begin (set! count (+ count 1))
                           (f m)))))    
    dispatch)
(define (inc a) (+ a 1))
(define s (make-monitored inc))
;3.3-3.4
(define (make-account balance password)
    (define counter 0)
    (define (call-the-cops) (display "calling"))
    (define (inc x)
        (if (= counter 2)
            (call-the-cops)
            (begin (set! counter (+ 1 counter))
                   (display "worry_password")
                   (newline))))
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   balance)
            (error "Insufficient funds" amount)))
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
    (define (dispatch m try)
        (cond ((not (= try password)) inc)
              ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "error" try))))
    dispatch)
;3.5
(define (square x) (* x x))
(define (random-in-range low high)
    (+ low (random (- high low))))
(define (try-once)
    (let ((x (random-in-range -1.0 1.0))
          (y (random-in-range -1.0 1.0)))
        (<= (+ (square x)
               (square y))
            1.0)))
(define (estimate-integral times)
    (*  4.0
        (monta-carlo times try-once)))
(define (monta-carlo times test)
    (define (iter times-remain pass)
        (cond ((= 0 times-remain) (/ pass times))
              ((test)
                (iter (- times-remain 1) (+ pass 1)))
              (else
                (iter (- times-remain 1) pass))))
    (iter times 0))
;3.6
(define (rand-new)
    (let ((x random-init))
        (lambda (m)
            (cond ((eq? m 'reset) (lambda (temp) (set! x temp)))
                  ((eq? m 'generate) (lambda () (begin (set! x (rand-update x)) x)))
                  (else (error "unkown message" m))))))
;3.7
(define (make-account balance password)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   balance)
            (error "Insufficient funds" amount)))
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
    (define (dispatch m try)
        (cond ((not (eq? try password)) (error "worry password" try))
              ((eq? m 'check) #t)
              ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "error" try))))
    dispatch)
(define (make-joint o-acc o-password new-password)
    (define (link m password)
        (if (= password new-password)
            (o-acc m o-password)
            (error "worry password" o-acc)))
    (if (o-acc 'check o-password)
        link
        (error "worry password" 0-acc)))
;3.8
(define f
    (let ((inner 1))
        (lambda (x)
            (if (= inner 0) inner
                (begin (set! inner x)
                       inner)))))
(define (show . s)
    (map (lambda (x) (begin (display x) (newline) x)) s))
; (+ (f 0) (f 1))为0，则scheme为从左到右求值