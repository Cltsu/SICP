(define (dd n)
    (cond ((> 3 n) n)
          (else (+ (dd (- n 1))
                   (dd (- n 2))
                   (dd (- n 3))))))

(define (d n)
    (define (iter i1 i2 i3 count)
        (cond ((> count n) i1)
              (else (iter (+ i1 i2 i3)
                    i1
                    i2
                    (+ count 1)))))
    (if ((> 3 n) n)
        (iter 3 2 1 3)))