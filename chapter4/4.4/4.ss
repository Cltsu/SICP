;4.1
(define (list-of-values exps env)
    (if (no-operands? exps)
        '()
        (let ((first (eval (first-operand exps) env)))
            (cons first
                  (list-of-values (rest-operands exps) env)))))
;4.2
;a)对于application的检测仅仅是pair？谓词，而该谓词对于自求值和变量之外的语句都是返回#t的，所以必须放在cond的最后。
;b)我们所有的语法都是建立在对语句的谓词判断和选择函数上，所以只需要修改application相关的函数
(define (application? exp)
    (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
(define (no-operands ops) (null? ops))
(define (first-operand ops) (cadr ops))
(define (rest-operands ops) (cddr ops))
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
;4.5 将该形式的cond转化为if语句是错误的，cond-predicate first会在转化之后的语句中出现两次，导致额外的计算，更重要的是两次的pred的结果不一定相同
;正确的做法应该是将cond作为特殊形式而非转化为if
;这是错误代码
(define (expand-clauses45 clauses)
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
;4.6
(define (eval46 exp env)
    (cond ((let? exp) (eval46 (let-combination exp) env))))
(define (let-defn exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (let-combination exp env)
    (let ((defn (let-defn exp))
          (body (let-body exp)))
        (let ((vars (map car defn))
              (exps (map cadr defn)))
            (cons (make-lambda vars body)
                  exps))))
;4.7
(define (eval47 exp env)
    (cond ((let? exp) (eval47 (let-combination exp) env))
          ((let*? exp) (eval47 (let*->nested-lets exp) env))))
(define (last-let-defn? defn) (null? (cdr defn)))
(define (make-let defn body)
    (cons 'let (cons defn body)))
(define (let*->nested-lets exp)
    (let ((defn (let-defn exp))
          (body (let-body exp)))
        (define (iter defns)
            (if (last-let-defn? defns)
                (make-let defns body)
                (make-let 
                    (list (car defns))
                    (list (iter (cdr defns))))))
        (iter defn)))
;4.8
;4.9
;循环可以用begin来实现，但是循环应该返回什么。循环应该只有纯副作用，例如赋值和输出，返回'ok
;4.10
;可以将scheme改为后缀表达式，只需要修改谓词和选择函数，而不用动eval和apply。
;4.11
(define (make-frame vars vals)
    (if (null? vars)
        '()
        (cons (cons (car vars) (car vals))
              (make-frame (cdr vars) (cdr vals)))))
(define (first-pair frame) (car frame))
(define (rest-pairs frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
    (cons (cons (var val))
          frame))
(define (lookup-var-val var env)
    (define (scan pairs)
        (cond ((null? pairs)
               (lookup-var-val var (enclosing-environment env)))
              ((eq? (car (first-pair pairs)) var)
               ((cdr (first-pair pairs))))
              (else (scan (rest-pairs pairs)))))
    (if (eq? env the-empty-environment)
        (error "can't find var" var)
        (let ((frame (first-frame env)))
            (scan frame))))
(define (set-var-val! var val env)
    (define (env-loop env)
        (define (scan pairs)
            (let ((cur (first-pair pairs))
                  (rest (rest-pairs pairs)))
                (cond ((null? pairs)
                       (env-loop (enclosing-environment env)))
                      ((eq? (car cur) var)
                       (set-cdr! cur val))
                      (else (scan rest)))))
        (if (eq? the-empty-environment env)
            (error "can't find var" var)
            (scan (first-frame env))))
    (env-loop env))
(define (define-var! var val env)
    (let ((frame (first-frame env)))
        (define (scan pairs)
            (let ((cur (first-pair pairs))
                  (rest (rest-pairs pairs)))
                (cond ((null? pairs)
                       (add-binding-to-frame! var val frame))
                      ((eq? var (car cur))
                       (set-cdr! cur val))
                      (else (scan rest)))))
        (scan frame)))
;4.12
(define (scan-and-do-something env find-op not-find-op var)
    (define (env-loop env)
        (define (scan vars vals)
            (cond ((null? vars)
                   (env-loop (enclosing-environment env)))
                  ((eq? var (car vars))
                   (find-op vals))
                  (else (scan (cdr vars) (cdr vals)))))
        (if (eq? env the-empty-environment)
            (not-find-op)
            (let ((frame (first-frame env)))
                (scan (frame-variables frame)
                      (frame-values frame)))))
    (env-loop env))
(define (set-variable-value! var val env)
    (define (find-op vals) (set-car! vals val))
    (define (not-find-op) (error "cant find var" var))
    (scan-and-do-something env find-op not-find-op var))
(define (define-variable! var val env)
    (let ((new-env (cons (first-env) the-empty-environment)))
        (define (find-op vals) (set-car! vals val))
        (define (not-find-op) (add-binding-to-frame! var val (first-frame new-env)))
        (scan-and-do-something new-env find-op not-find-op var)))
(define (lookup-variable-value var env)
    (define (find-op vals) (car vals))
    (define (not-find-op) (error "cant find var" var))
    (scan-and-do-something env find-op not-find-op var))
;4.13
;在一个函数中使用的一个约束，其是从当前环境不断向外环境寻找而找到的第一个约束
;取消绑定也应该使用相同的规则
(define (make-unbound! var env)
    (define (env-loop env)
        (define (scan vars vals lastvars lastvals)
            (cond ((null? vars)
                   (env-loop (enclosing-environment env)))
                  ((eq? var (car vars))
                   (set-cdr! last-vars (cdr vars))
                   (set-cdr! last-vals (cdr vals)))
                  (else (scan (cdr vars) (cdr vals)))))
        (if (eq? env the-empty-environment)
            (error "unbound var" var)
            (let ((frame (first-frame env))
                  (vars (frame-variables (first-frame env)))
                  (vals (frame-values (first-frame env))))
                (cond ((null? vars)
                       (env-loop (enclosing-environment env)))
                      ((eq? var (car vars))
                       (set-car! frame (cdr vars))
                       (set-cdr! frame (cdr vals)))
                      (else (scan (cdr vars) (cdr vals) vars vals))))))
    (env-loop env))
;4.14
;我们能使用primitive过程，是因为我们直接使用了部分底层的scheme的数据结构或者定义，比如car，#t。
;我们可以将布尔值设为'true-mod,'false-mod，但是我们就无法将scheme的 = 符号作为primitive过程引入，因为等号返回的是#t\#f(chez scheme)。
;现在回到原题，我们将用底层scheme和新scheme区分两个层次
;关键在于map的参数能否被底层scheme的apply识别，map的第一个参数为函数
;但不是底层scheme的函数，而是我们实现的新scheme的函数,而底层scheme的apply则无法识别新scheme里的函数

;4.15
;停机问题，
;求值(try try)代表着让halts?判断(try try)会不会停机，
;如果halts?判断(try try)会停机，则会转入(run-forever)。
;不过此时是什么在(run-forever)?，是被halts判断为会停机的(try try)，
;被判为会停机的过程却进入了(run-forever)
;这里就产生了矛盾
;halts?判断不停机也会产生相似的矛盾。
;这表明了halts?是不存在的。

;4.16
;a)
(define (lookup-variable-value var env)
    (define (env-loop env)
        (define (scan vars vals)
            (cond ((null? vars) 
                   (env-loop (enclosing-environment env)))
                  ((eq? var (car vars))
                   (if (eq? (car vals) '*unassigned*)
                       (error "unassigned var" var)
                       (car vals)))
                  (else (scan (cdr vars) (cdr vals)))))
        (if (eq? env the-empty-environment)
            (error "can't find var" var)
            (let ((frame (first-frame env)))
                (scan (frame-variables frame)
                      (frame-values frame)))))
    (env-loop env))
;b)未测试
(define (scan-out-defines exps)
    (define (div exps defns exprs)
        (cond ((null? (cons defns exprs)))
              ((definition? (car exps))
                (div (cdr exps) (cons (car exps) defns) exprs))
              (else (div (cdr exps) defns (cons (car exps) exprs)))))
    (define (make-let-trans defns exprs)
        (let ((vars-exps (map (lambda (x) (list (definition-variable x) '*unassigned*)) defns))
              (set-exps (map (lambda (x) (set-car! x 'set!)) defns)))
            (list (cons 'let
                        (cons vars-exps
                              (append set-exps exprs))))))
    (let ((div-exps (div exps '() '())))
        (make-let-trans (car div-exps) (cdr div-exps))))
;c)
;放在make-procedure好，放在procedure-body会在每次使用函数时调用scan-out-defines，开销太大
;4.17
;因为多了一层let，会多一层环境
;4.18
;会出错，因为定义dy会取y的第一个元素，但此时y为unassigned
;正文方式没问题
;4.19
;我使用的scheme版本执行该过程报错了。
;可以考虑用图论方法，通过依赖关系建立图，拓扑排序来确定先后次序。
;4.20
;不想做
;4.21
;a)这个函数就是多套了一层lambda而将函数隐式地绑定为fact\ft
((lambda (n)
    ((lambda (fact) (fact fact n 1 1))
        (lambda (ft k one two)
            (cond ((= k 1) one)
                  ((= k 2) two)
                  (else (ft ft (- k 1) two (+ two one)))))))
6)
;b)
(define (f x)
    ((lambda (even? odd?)
        (even? even? odd? x))
     (lambda (ev? od? n)
        (if (= n 0) #t (od? ev? od? (- n 1))))
     (lambda (ev? od? n)
        (if (= n 0) #f (ev? ev? od? (- n 1))))))
;4.22
;let只是语法糖，所以实现let不需要额外的analyze-let
;直接在analyze函数中加上
;((let? exp) (analyze (let->combination exp)))
;4.23
;如题干所说，alyssa的版本没有将序列化在语法分析中实现
;虽然它们运行起来都是正确的
