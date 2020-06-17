(define (element-of-set? x set)
    (cond ((null? set) #f)
          ((equal? x (car set)) #t)
          (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
    (if (element-of-set? x set) set
        (cons x set)))

;2.59
(define (union-set s1 s2)
    (cond ((null? s1) s2)
          ((element-of-set? (car s1) s2)
            (union-set (cdr s1) s2))
          (else (union-set (cdr s1) (cons (car s1) s2)))))
;2.60
(define (element-of-set-mod x set);不变
    (element-of-set x set))
(define (adjoin-set-mod x set)
    (cons x set))
(define (union-set-mod s1 s2)
    (if (null? s1) s2
        (union-set-mod (cdr s1) (cons (car s1) s2))))
; (define (intersection-set-mod s1 s2);不变
;     (intersection s1 s2))
;添加元素速度会变快，其他速度不变
;2.61
(define (adjoin-set261 x set)
    (define (element-of-set-sorted? x set)
        (cond ((null? set) #f)
              ((> x (car set)) #f)
              ((= x (car set)) #t)
              (else (element-of-set-sorted? x (cdr set)))))
    (if (element-of-set-sorted? x set) set
        (cons x set)))
;2.62
(define (union-set s1 s2)
    (cond ((null? s1) s2)
          ((null? s2) s1)
          ((= (car s1) (car s2)) (cons (car s1) (union-set (cdr s1) (cdr s2))))
          ((> (car s1) (car s2)) (cons (car s2) (union-set s1 (cdr s2))))
          (else (cons (car s1) (union-set (cdr s1) s2)))))
;
;
;
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
    (list entry left right))
(define (element-of-tree? x set)
    (cond ((null? set) #f)
          ((= x (entry set)) #t)
          ((> x (entry set)) (element-of-tree? x (right-branch set)))
          ((< x (entry set)) (element-of-tree? x (left-branch set)))))
;2.63
;a)结果一样，均为(1 2 3 4 5 6 7)
;b)第二种快，第二种为O(n),第一种为O(nlogn)
;2.64
;输入必须得是有序表，不是动态平衡，比较简单
;2.65
(define (union-tree t1 t2)
    (let ((l1 (tree->list-1 t1))
          (l2 (tree->list-2 t2)))
         (list->tree (union-set l1 l2))))
(define (intersection t1 t2)
    (let ((l1 (tree->list-1 t1))
          (l2 (tree->list-2 t2)))
         (list->tree (intersection-set l1 l2))))
;2.66 不想做