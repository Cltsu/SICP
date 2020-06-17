(define (memq item x)
    (cond ((null? x) #f)
          ((eq? item (car x)) x)
          (else (memq item (cdr x)))))
;2.53
;(a b c)
;((george))
;((y1 y2))
;(y1 y2)
;#f
;#f
;#t
;2.54
(define (equal-? l1 l2)
    (cond ((and (null? l1) (null? l2)) #t)
          ((or (null? l1) (null? l2)) #f)
          (else (and (eq? (car l1) (car l2))
                     (equal-? (cdr l1) (cdr l2))))))
;2.55
;(quote (quote abracadabra)),第二个quote被视为一个符号，而不是一个函数
;
;
;
