(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))
(define (filter predicate sequence)
    (cond ((null? sequence) '())
          ((predicate (car sequence))
           (cons (car sequence)
                 (filter predicate (cdr sequence))))
          (else (filter predicate (cdr sequence)))))
;2.33
(define (map- p sequence)
    (accumulate (lambda (x y) (cons (p x) y));此处的y为并非为initial，而是accumulate op initial (cdr seq)
                '()
                sequence))
(define (append- seq1 seq2)
    (accumulate (lambda (x y) (cons x y)) seq2 seq1))
(define (length- sequence)
    (accumulate (lambda (x y) (+ x 1)) 0 sequence))
;2.34
(define (horner-eval x cs)
    (accumulate (lambda (this-coeff higher-terms)
                        (+ (* higher-terms x)
                           this-coeff))
                (car cs)
                (cdr cs)))
;2.35
(define (count-leaves t)
    (accumulate (lambda (x y) (if (pair? x)
                                  (+ (count-leaves x) y)
                                  (+ 1 y)))
                0
                t))
(define (count-leaves- t)
    (accumulate (lambda (x y) (+ x y))
                0
                (map (lambda (x) (if (pair? x)
                                     (count-leaves- x)
                                     1))
                      t)))
;2.36
(define (accumulate-n op init seqs)
    (if (null? (car seqs)) '()
        (cons (accumulate op init (map (lambda (x) (car x)) seqs))
              (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))
;2.37
;矩阵乘法记不到了
;2.38
(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest)
            result
            (iter (op result (car rest))
                  (cdr rest))))
    (iter initial sequence))
(define (fold-right op initial sequence)
    (accumulate op initial sequence))
;op满足交换律，left从左边开始每一个元素和inti组合，right则是从右边，考虑对seq的保序性，op的参数的位置也有变化。
;2.39
(define (reverse-r sequence)
    (fold-right (lambda (x y) (append y (list x))) '() sequence))
(define (reverse-l sequence)
    (fold-left (lambda (x y) (cons y x) '() sequence)))
;
;
;
(define (interval a b)
    (if (> a b) '()
        (cons a (interval (+ a 1) b))))
(define (flatmap proc seq)
    (accumulate append '() (map proc seq)))
(define (permutations s)
    (if (null? s) (list '())
        (flatmap (lambda (x)
                    (map (lambda (p) (cons x p))
                         (permutations (remove x s))))
                 s)))
(define (remove item sequence)
    (filter (lambda (x) (not (= x item)))
            sequence))
;2.40
(define (unique-pairs n)
    (flatmap (lambda (x)
                     (map (lambda (y) (list y x))
                          (interval 1 (- x 1))))
             (interval 1 n)))
(define (prime-sum-pairs n)
    (filter prime-sum?
            (unique-pairs n)))
;2.41
(define (unique-three-pairs n)
    (flatmap (lambda (x)
                     (map (lambda (y) (cons y x))
                     (interval 1 (- (car x) 1))))
             (unique-pairs n)))
(define (three-sum n)
    (filter (lambda (x) (= n (accumulate + 0 x)))
            (unique-three-pairs n)))
;2.42
(define empty-board '());没有解，而不是没有棋子
(define (safe? k positions);k有什么用吗
    (define (test new rest op)
        (accumulate (lambda (x y) 
                            (and y (not (= (op new) (op x)))))
                    #t
                    rest))
    (if (= k 1) #t
        (and (test (car positions) (cdr positions) (lambda (x) (cadr x)))
             (test (car positions) (cdr positions) (lambda (x) (+ (car x) (cadr x))))
             (test (car positions) (cdr positions) (lambda (x) (- (car x) (cadr x)))))))
(define (adjoin-position new-row k rest-of-queens)
    (cons (list k new-row) rest-of-queens))
(define (queens board-size)
    (define (queen-cols k)
        (if (= k 0)
            (list empty-board)
            (filter
                (lambda (positions) (safe? k positions))
                (flatmap
                    (lambda (rest-of-queens)
                        (map (lambda (new-row)
                                     (adjoin-position new-row k rest-of-queens))
                            (interval 1 board-size)))
                    (queen-cols (- k 1))))))
    (queen-cols board-size))
;2.43
;复杂度变高了
;
;2.2.4
;🔨画家啊，不想做