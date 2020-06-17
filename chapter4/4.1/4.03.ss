;4.3
(define (eval exp env)
    (cond ((self-evaluating? exp) exp)
          ((variable? exp) (lookup-variable-value exp env))
          ((get 'eval (car exp)
            (apply (get 'eval (car exp)) exp env)))
          ((application? exp)
            (apply (eval (operator exp) env)
                   (list-of-values (operands exp) env)))
          (else
            (error "Unknown expression type -- EVAL" exp))))
