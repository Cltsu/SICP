(define (memo-proc proc)
    (let ((already-run? #f) (result #f))
        (lambda ()
            (if (not already-run?)
                (begin (set! result (proc))
                       (set! already-run? #t))
                result))))
; (define (delay proc)
;     (memo-proc (lambda () proc)))
(define (dalay proc)
    (lambda () proc))
(define (force delay-proc)
    (delay-proc))
(define (cons-stream a b)
    (cons a (delay b)))
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
(define (stream-map proc s)
    (if (stream-null? s)
        the-empty-stream
        (cons-stream (proc (stream-car s))
                     (stream-map proc (stream-cdr s)))))
(define (stream-ref s n)
    (if (= n 0)
        (stream-car n)
        (stream-ref (stream-cdr s) (- n 1))))
;3.50
(define (stream-map-formal proc . args)
    (cond ((null? args) the-empty-stream)
          ((stream-null? (car args)) the-empty-stream)
          (else (cons-stream (apply proc (map stream-car args))
                             (apply stream-map-formal
                                    (cons proc
                                          (map stream-cdr args)))))))
;3.51
;0 5 7
;3.52
;1 6 10
;136 210
;会变
(define (show . s)
    (map (lambda (x) (begin (display x) (newline) x)) s))
(define (show-stream s)
    (if (stream-null? s) '()
        (begin (display (stream-car s))
               (show-stream (stream-cdr s)))))
(define s4 (stream-enumerate-interval 1 1000000000000000000))
(define s5 (stream-enumerate-interval 1 10))
; (show-stream (stream-map-formal + s4 s5))
(display (stream-car s4))
(exit)