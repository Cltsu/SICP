;4.5 将该形式的cond转化为if语句是错误的，cond-predicate first会在转化之后的语句中出现两次，导致额外的计算，更重要的是两次的pred的结果不一定相同
(define (expand-clauses45 clauses);这是错误代码
    (if (null? clauses)
        'false
        (let ((first (car clauses))
              (rest (cdr clauses)))
            (if (cond-else-clause? first)
                (if (null? rest)
                    (sequence->exp (cond-actions first))
                    (error "ELSE clause isn't last -- COND->IF" rest))
                (let ((actions (cond-actions first)))
                    (if (eq? '=> (car action))
                        (make-if (cond-predicate first)
                            (list (sequence->exp (cdr actions)) (cond-predicate first))
                            (expand-clauses45 rest))
                        (make-if (cond-predicate first)
                            (sequence->exp actions)
                            (expand-clauses45 rest))))))))
;正确的做法应该是将cond作为特殊形式而非转化为if