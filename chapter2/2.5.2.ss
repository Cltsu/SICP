(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(define (attach-tag type-tag contents)
    (cons type-tag contents))
(define (type-tag datum)
    (if (pair? datum)
        (car datum)
        (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
    (if (pair? datum)
        (cdr datum)
        (error "Bad tagged datum -- CONTENTS" datum)))
(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (error "No method for these types" 
                       (list op type-tags))))))
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (install-scheme-number-package)
    (define (equ? x y) (= x y))
    (define (tag x)
        (attach-tag 'scheme-number x))
    (put 'add '(scheme-number scheme-number)
        (lambda (x y) (tag (+ x y))))
    (put 'sub '(scheme-number scheme-number)
        (lambda (x y) (tag (- x y))))
    (put 'mul '(scheme-number scheme-number)
        (lambda (x y) (tag (* x y))))
    (put 'div '(scheme-number scheme-number)
        (lambda (x y) (tag (/ x y))))
    (put 'equ? '(scheme-number scheme-number) equ?)
    (put 'zero '(scheme-number)
        (lambda (x) (= 0 x)))
    (put 'make 'scheme-number;为什么这个不像上一个一样加括号，因为这个函数不是给apply-generic调用的
        (lambda (x) (tag x)))
    'done)

(define (install-rectangular-package)

  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))

  (define (make-from-real-imag x y) (cons x y))

  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))

  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))

  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)

  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))

'done)

(define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))


(define (install-polar-package)

  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))

  (define (make-from-mag-ang r a) (cons r a))

  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))

  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))

  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)

  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))

'done)

(define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (install-complex-package)

    ;; imported procedures from rectangular and polar packages
    (define (make-from-real-imag x y)
        ((get 'make-from-real-imag 'rectangular) x y))

    (define (make-from-mag-ang r a)
        ((get 'make-from-mag-ang 'polar) r a))

    ;; internal procedures
    (define (add-complex z1 z2)
        (make-from-real-imag (+ (real-part z1) (real-part z2))
                             (+ (imag-part z1) (imag-part z2))))

    (define (sub-complex z1 z2)
        (make-from-real-imag (- (real-part z1) (real-part z2))
                             (- (imag-part z1) (imag-part z2))))

    (define (mul-complex z1 z2)
        (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                           (+ (angle z1) (angle z2))))

    (define (div-complex z1 z2)
        (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                           (- (angle z1) (angle z2))))

    (define (equ? z1 z2)
        (and (= (real-part z1)
                (real-part z2))
             (= (imag-part z1)
                (imag-part z2))))
    ;; interface to rest of the system
    (define (tag z)
        (attach-tag 'complex z))
    (put 'zero '(complex) 
        (lambda (x) (and (= 0 (real-part x))
                         (= 0 (imag-part x)))))
    (put 'equ '(complex complex) equ?)
    (put 'add '(complex complex)
        (lambda (z1 z2)
            (tag (add-complex z1 z2))))

    (put 'sub '(complex complex)
        (lambda (z1 z2)
            (tag (sub-complex z1 z2))))

    (put 'mul '(complex complex)
        (lambda (z1 z2)
            (tag (mul-complex z1 z2))))

    (put 'div '(complex complex)
        (lambda (z1 z2)
            (tag (div-complex z1 z2))))

    (put 'make-from-real-imag 'complex
        (lambda (x y)
            (tag (make-from-real-imag x y))))

    (put 'make-from-mag-ang 'complex
        (lambda (x y)
            (tag (make-from-mag-ang x y))))

'done)

(define (make-complex-from-real-imag x y)
    ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
    ((get 'make-from-mag-ang 'complex) r a))
;2.77
;real-part是两种不同复数实现的通用过程
;apply-generic调用两次，一次分配给通用magnitude，一次分配给rectangular内部的magnitude
;2.78
(define (install-scheme-number-package)
    (define (tag x)
        (attach-tag 'scheme-number x))
    (put 'add '(scheme-number scheme-number)
        (lambda (x y) (tag (+ x y))))
    (put 'sub '(scheme-number scheme-number)
        (lambda (x y) (tag (- x y))))
    (put 'mul '(scheme-number scheme-number)
        (lambda (x y) (tag (* x y))))
    (put 'div '(scheme-number scheme-number)
        (lambda (x y) (tag (/ x y))))
    (put 'make 'scheme-number
        (lambda (x) (tag x)))
    'done)
(define (attach-tag type-tag contents)
    (if (number? contents) contents)
        (cons type-tag contents))
(define (type-tag datum)
    (cond   ((number? datum) 'scheme-number)
            ((pair? datum) (car datum))
            (else (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
    (cond   ((number? datum) datum)
            ((pair? datum) (cdr datum))
            (else (error "Bad tagged datum -- CONTENTS" datum))))
;2.79
(define (equ? x y)
    (apply-generic 'euq x y))
;2.80
(define (zero? x)
    (apply-generic 'zero x))
;2.81
;a)安装了强制过程后，如果两个参数类型相同而又找不到proc，会进入死循环
;b)如果说有t1->t2、t2->t1两种变换，那么该程序可能导致忽略第二种；如果只有一种有对应的op，那么有可能会忽略这个op
;c)
(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (if (and  (= (length args) 2)
                          (eq? (car args)
                               (cadr args)))
                    (let ((type1 (car type-tags))
                          (type2 (cadr type-tags))
                          (a1 (car args))
                          (a2 (cadr args)))
                        (let ((t1->t2 (get-coercion type1 type2))
                              (t2->t1 (get-coercion type2 type1)))
                            (cond (t1->t2
                                    (apply-generic op (t1->t2 a1) a2))
                                  (t2->t1
                                    (apply-generic op a1 (t2->t1 a2)))
                                  (else
                                    (error "No method for these types"
                                            (list op type-tags))))))
                    (error "No method for these types"
                            (list op type-tags)))))))
;2.82
;同2.81的程序，如果只是检查能否统一到同一个类型而不检查是否存在函数，那么在能够统一到多个类型的情况下可能会出现合法操作没被调用的情况。
;2.83 - 2.86 不想做
