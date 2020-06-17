;4.1
(define (list-of-values exps env)
    (if (no-operands? exps)
        '()
        (let ((first (eval (first-operand exps) env)))
            (cons first
                  (list-of-values (rest-operands exps) env)))))