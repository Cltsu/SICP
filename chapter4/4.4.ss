;4.4
(define (or? exp) (tagged-list? exp 'or))
(define (or-actions exp) (cdr exp))
(define (first-orexp exps) (car exps))
(define (rest-orexps exps) (cdr exps))
(define (last-orexp? exps) (null? (cdr exps)))
;作为特殊形式
(define (eval44 exp env)
    (cond ((or? exp) (eval-or exp env));added
          ((and? exp) (eval-and exp env))));added
(define (eval-or exp env)
    (eval-or-exps (or-actions exp) env))
(define (eval-or-exps exps env)
    (let ((first (first-orexp exps))
          (rest (rest-orexps exps)))
        (if (true? (eval first env))
            'true
            (if (last-orexp? exp)
                'false
                (eval-or-exps rest env)))))
;作为派生表达式
(define (eval44 exp env)
    (cond ((and? exp) (eval44 (and-if exp) env))
          ((or? exp) (eval44 (or-if exp) env))))
(define (or-if exp)
    (expand-or (or-actions exp)))
(define (expand-or exps)
    (if (null? exps)
        'false
        (let ((first (first-orexp exps))
              (rest (rest-orexp exps)))
            (if (last-orexp? exps)
                (sequence->exp first)
                (make-if (sequence-exp first)
                         'true
                         (expand-or rest))))))