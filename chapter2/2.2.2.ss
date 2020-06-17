(define (length items)
    (if (null? items)
        0
        (+ 1 (length (cdr items)))))
(define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append (cdr list1) list2))))
;2.17
(define (last-pair l)
    (if (null? (cdr l))
        (car l)
        (last-pair (cdr l))))
;2.18 
(define (reverse l);可以直接递归吗？
    (define (reverse- list1 list2)
        (if (null? list1) list2
            (reverse- (cdr list1) (cons (car list1) list2))))
    (reverse- l '()));如何建立一个空list？
;2.19不想做
;2.20
(define (same-parity . l)
    (define (partition l remain)
        (cond ((null? l) (list ))
              ((= remain (remainder (car l) 2)) (cons (car l) (partition (cdr l) remain)))
              (else (partition (cdr l) remain))))
    (if (null? l) '()
        (partition l (remainder (car l) 2))))
;2.21
(define (square x) (* x x))
(define (square-list l)
    (if (null? l) '()
        (cons (square (car l))
          (square-list (cdr l)))))
(define (square-list- l)
    (map square l))
;2.22
;向一个list添加元素的朴素方法只有加在前面，而对answer的增长是向后的，所以要么使用reverse，要么就append
;第二个解法则是完全错误的，(cons answer (...))，answer作为一个list放在了car位，而后面的作为一个数放在了cdr位，不能构成list。

;2.23感觉用lambda实现不太好
(define (for-each- f l)
    (if (null? l) #t
        ((lambda (x) (f (car x)) (for-each- f (cdr l))) l)))
;2.24
;我画出来了，但是该怎么贴到这上面？
;2.25?
(define p1 (list 1 3 (list 5 7) 9))
(define p2 (list (list 7)))
(define p3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
;(car (cdaddr p1))
;(caar p2)
;(cadadr (cadadr (cadadr p3)))
;2.26
;(1 2 3 4 5 6)
;((1 2 3) 4 5 6),因为cdr连着一个list，和list的结构相同，所以将(1 2 3)解释为list的第一个元素。
;((1 2 3) (4 5 6))
;2.27
(define (deep-reverse l)
    (define (dr l ret)
        (if (null? l) ret
            (dr (cdr l) (cons (if (pair? (car l))
                                  (deep-reverse (car l))
                                  (car l)) 
                               ret))))
    (dr l '()))
;2.28 复杂度也太高了？
(define (fringe tree)
    (cond ((null? tree) '())
          ((not (pair? tree)) (list tree))
          (else (append (fringe (car tree))
                        (fringe (cdr tree))))))
; ;2.29
(define (make-mobile left right)
    (list left right))
(define (make-branch length structure)
    (list length structure))
;a
(define (left-branch m) (car m))
(define (right-branch m) (cadr m))
(define (branch-length b) (car b))
(define (branch-structure b) (cadr b))
;b
(define s1 (make-branch 1 10))
(define s2 (make-branch 2 5))
(define s3 (make-branch 3 10))
(define s4 (make-branch 10 3))
(define m1 (make-mobile s1 s2))
(define m2 (make-mobile s3 s4))
(define s5 (make-branch 5 m1))
(define s6 (make-branch 5 m2))
(define m3 (make-mobile s5 s6))
(define (have-next-struct? b) (pair? (branch-structure b)))
(define (total-weight m)
    (define (branch-iter s)
        (if (have-next-struct? s)
            (total-weight (branch-structure s))
            (branch-structure s)))
    (+ (branch-iter (left-branch m))
       (branch-iter (right-branch m))))

;c
(define (isbalance m)
    (define (isbalance-branch? b)
        (if (have-next-struct? b)
            (isbalance (branch-structure b))
            #t))
    (define (w-l b)
        (* (branch-length b)
           (if (have-next-struct? b)
               (total-weight (branch-structure b))
               (branch-structure b))))
    (and (= (w-l (left-branch m))
            (w-l (right-branch m)))
         (isbalance-branch? (left-branch m))
         (isbalance-branch? (right-branch m))))
;d
; 将选择函数中cadr换成cdr
; (define (make-mobile left right)
;     (cons left right))
; (define (make-branch length structure)
;     (cons length structure))
; (define (left-branch m) (car m))
; (define (right-branch m) (cdr m))
; (define (branch-length b) (car b))
; (define (branch-structure b) (cdr b))

;2.30
(define (square-tree tree)
    (cond ((null? tree) '())
          ((not (pair? tree)) (square tree))
          (else (cons (square-tree (car tree))
                      (square-tree (cdr tree))))))
(define (square-tree-map tree);map不会调用nil
    (map (lambda (sub-tree)
                 (if (pair? sub-tree)
                     (square-tree-map sub-tree)
                     ((lambda (x) (* x x)) sub-tree)))
         tree))
;2.31
(define (tree-map f tree)
    (map (lambda (sub-tree)
                 (if (pair? sub-tree)
                     (tree-map f sub-tree)
                     (f sub-tree)))
         tree))
;2.32
(define (subset s)
    (if (null? s) (list '())
        (let ((ss (subset (cdr s))))
            (append (map (lambda (x) (cons (car s) x)) ss)
                    ss))))