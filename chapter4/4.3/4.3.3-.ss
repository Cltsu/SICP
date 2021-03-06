(define (ambeval exp env succeed fail)
    ((analyze exp) env succeed fail))
(define (analyze exp)
    (cond ((self-evaluating? exp)
           (analyze-self-evaluating exp))
          ((quoted? exp) (analyze-quoted exp))
          ((variable? exp) (analyze-variable exp))
          ((amb? exp) (analyze-amb exp))
          ((require? exp) (analyze-require exp))
          ((permanent-set? exp) (analyze-permanent-set exp))
          ((if-fail? exp) (analyze-if-fail exp))
          ((assignment? exp) (analyze-assignment exp))
          ((definition? exp) (analyze-definition exp))
          ((let? exp) (analyze (let-combination exp)))
          ((if? exp) (analyze-if exp))
          ((lambda? exp) (analyze-lambda exp))
          ((begin? exp) (analyze-sequence (begin-actions exp)))
          ((cond? exp) (analyze (cond->if exp)))
          ((application? exp) (analyze-application exp))
          (else 
            (error "unkown expression type" exp))))
;简单表达式
(define (analyze-self-evaluating exp)
    (lambda (env succeed fail) (succeed exp fail)))
(define (analyze-quoted exp)
    (let ((qval (text-of-quotation exp)))
        (lambda (env succeed fail) (succeed qval fail))))
(define (analyze-variable exp)
    (lambda (env succeed fail) (succeed (lookup-variable-value exp env) fail)))
(define (analyze-lambda exp)
    (let ((vars (lambda-parameters exp))
          (bproc (analyze-sequence (lambda-body exp))))
        (lambda (env succeed fail)
            (succeed (make-procedure vars bproc env) fail))))
;定义和赋值
(define (analyze-assignment exp)
    (let ((var (assignment-variable exp))
          (vproc (analyze (assignment-value exp))))
        (vproc env
               (lambda (val fail2)
                    (let ((old-value (lookup-variable-value var env)))
                        (set-variable-value! var val env)
                        (succeed 'ok
                                 (lambda ()
                                    (set-variable-value! var old-value env)
                                    (fail2)))))
               fail)))
(define (analyze-definition exp)
    (let ((var (definition-variable exp))
          (vproc (analyze (definition-value exp))))
        (lambda (env succeed fail)
            (vproc env
                   (lambda (val fail2)
                        (define-variable! var val env)
                        (succeed 'ok fail2))
                    fail))))
;条件和序列
(define (analyze-if exp)
    (let ((pproc (analyze (if-predicate exp)))
          (cproc (analyze (if-consequent exp)))
          (aproc (analyze (if-alternative exp))))
        (lambda (env succeed fail)
            (pproc env
                   (lambda (pred-value fail2)
                        (if (true? pred-value)
                            (cproc env succeed fail2)
                            (aproc env succeed fail2)))
                   fail))))
(define (analyze-sequence exps)
    (define (sequentially a b)
        (lambda (env succeed fail)
            (a env
               (lambda (a-value fail2)
                    (b env succeed fail2))
               fail)))
    (define (loop first-proc rest-procs)
        (if (null? rest-procs)
            first-proc
            (loop (sequentially first-proc (car rest-procs))
                  (cdr rest-procs))))
    (let ((procs (map analyze exps)))
        (if (null? procs)
            (error "Empty sequence" '()))
        (loop (car procs) (cdr procs))))
;过程应用
(define (analyze-application exp)
    (let ((fproc (analyze (operator exp)))
          (aprocs (map analyze (operands exp))))
        (lambda (env succeed fail)
            (fproc env
                   (lambda (proc fail2)
                        (get-args aprocs
                                  env
                                  (lambda (args fail3)
                                        (execute-application proc args succeed fail3))
                                  fail2))
                   fail))))
(define (get-args aprocs env succeed fail)
    (if (null? aprocs)
        (succeed '() fail)
        ((car aprocs) env
                      (lambda (arg fail2)
                            (get-args (cdr aprocs)
                                      env
                                      (lambda (args fail3)
                                            (succeed (cons arg args)
                                            fail3))
                                      fail2))
                      fail)))
(define (execute-application proc args succeed fail)
    (cond ((primitive-procedure? proc)
           (succeed (apply-primitive-procedure proc args)
                    fail))
          ((compound-procedure? proc)
           ((procedure-body proc)
                (extend-environment (procedure-parameters proc)
                                    args
                                    (procedure-environment proc))
                succeed
                fail))
          (else
            (error "unkown procedure type" proc))))
;amb表达式
(define (analyze-amb exp)
    (let ((cprocs (map analyze (amb-choices exp))))
        (lambda (env succeed fail)
            (define (try-next choices)
                (if (null? choices)
                    (fail)
                    ((car choices) env
                                   succeed
                                   (lambda ()
                                        (try-next (cdr choices))))))
            (try-next cprocs))))
;
(define (self-evaluating? exp)
    (cond ((number? exp) #t)
          ((string? exp) #t)
          (else #f)))
(define (variable? exp) (symbol? exp))
(define (quoted? exp)
    (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cdr exp))
(define (tagged-list? exp tag)
    (if (pair? exp)
        (eq? (car exp) tag)
        #f))
;assignment
(define (assignment? exp)
    (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
;definition
(define (definition? exp)
    (tagged-list? exp 'define))
(define (definition-variable exp)
    (if (symbol? (cadr exp))
        (cadr exp)
        (caadr exp)))
(define (definition-value exp)
    (if (symbol? (cadr exp))
        (caddr exp)
        (make-lambda (cdadr exp)
                     (cddr exp))))
;lambda
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
    (cons 'lambda (cons parameters body)))
;if
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
    (if (not (null? (cdddr exp)))
        (cadddr exp)
        (list 'quote #f)))
(define (make-if predicate consequent alternative)
    (list 'if predicate consequent alternative))
;begin
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
    (cond ((null? seq) seq)
          ((last-exp? seq) (first-exp seq))
          (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))
;application
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
;cond
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
    (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
    (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
    (if (null? clauses)
        '#f
        (let ((first (car clauses))
              (rest (cdr clauses)))
            (if (cond-else-clause? first)
                (if (null? rest)
                    (sequence->exp (cond-actions first))
                    (error "ELSE clause isn't last -- COND->IF" rest))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest))))))
;谓词检测
(define (true? x) (not (eq? x '#f)))
(define (false? x) (eq? x '#f))
;过程的表示
(define (make-procedure parameters body env)
    (list 'procedure parameters body env))
(define (compound-procedure? p)
    (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))
;环境 作为frame的表
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
;frame 表示一个单独的环境
(define (make-frame variable values)
    (cons variable values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
    (set-car! frame (cons var (car frame)))
    (set-cdr! frame (cons val (cdr frame))))
(define (extend-environment vars vals base-env)
    (if (= (length vars) (length vals))
        (cons (make-frame vars vals) base-env)
        (error "can't match vals and vars" vars vals)))
(define (lookup-variable-value var env)
    (define (env-loop env)
        (define (scan vars vals)
            (cond ((null? vars) 
                   (env-loop (enclosing-environment env)))
                  ((eq? var (car vars))
                   (car vals))
                  (else (scan (cdr vars) (cdr vals)))))
        (if (eq? env the-empty-environment)
            (error "can't find var" var)
            (let ((frame (first-frame env)))
                (scan (frame-variables frame)
                      (frame-values frame)))))
    (env-loop env))
(define (set-variable-value! var val env)
    (define (env-loop env)
        (define (scan vars vals)
            (cond ((null? vars)
                   (env-loop (enclosing-environment env)))
                  ((eq? var (car vars))
                   (set-car! vals val))
                  (else (scan (cdr vars) (cdr vals)))))
        (if (eq? env the-empty-environment)
            (error "unbound var" var)
            (let ((frame (first-frame env)))
                (scan (frame-variables frame)
                      (frame-values frame)))))
    (env-loop env))
(define (define-variable! var val env)
    (let ((frame (first-frame env)))
        (define (scan vars vals)
            (cond ((null? vars)
                   (add-binding-to-frame! var val frame))
                  ((eq? var (car vars))
                   (set-car! vals val))
                  (else (scan (cdr vars) (cdr vals)))))
        (scan (frame-variables frame)
              (frame-values frame))))
;primitive过程
(define (primitive-procedure? proc)
    (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define primitive-procedures
    (list (list 'car car)
          (list 'cdr cdr)
          (list 'cons cons)
          (list 'null? null?)
          (list '+ +)
          (list '= =)
          (list '/ /)
          (list 'null? null?)
          (list '() '())
          (list 'list list)
          (list 'map map)
          (list 'not not)))
(define (primitive-procedure-names)
    (map car primitive-procedures))
(define (primitive-procedure-objects)
    (map (lambda (proc) (list 'primitive (cadr proc)))
         primitive-procedures))
(define (apply-primitive-procedure proc args)
    (apply
        (primitive-implementation proc) args))
;模拟运行
(define input-prompt "Amb-Eval input:")
(define output-prompt "Amb-Eval output:")
(define (driver-loop)
    (prompt-for-input input-prompt)
    (let ((input (read)))
        (let ((output (eval input the-global-environment)))
            (announce-output output-prompt)
            (user-print output)))
    (driver-loop))
(define (driver-loop)
    (define (interval-loop try-again)
        (prompt-for-input input-prompt)
        (let ((input (read)))
            (if (eq? input 'try-again)
                (try-again)
                (begin
                    (newline)
                    (display ";;; Starting a new problem")
                    (ambeval input
                             the-global-environment
                             (lambda (val next-alternative)
                                (announce-output output-prompt)
                                (user-print val)
                                (interval-loop next-alternative))
                             (lambda ()
                                (announce-output ";;; There are no more values of")
                                (user-print input)
                                (driver-loop)))))))
        (interval-loop
            (lambda ()
                (newline)
                (display ";;; There is no current problem")
                (driver-loop))))
(define (prompt-for-input string)
    (newline) (newline) (display string) (newline))
(define (announce-output string)
    (newline) (display string) (newline))
(define (user-print object)
    (if (compound-procedure? object)
        (display (list 'compound-procedure
                       (procedure-parameters object)
                       (procedure-body object)))
        (display object)))
;全局环境
(define (setup-environment)
    (let ((initial-env
            (extend-environment (primitive-procedure-names)
                                (primitive-procedure-objects)
                                the-empty-environment)))
        (define-variable! '#t #t initial-env)
        (define-variable! '#f #f initial-env)
        initial-env))
(define the-global-environment (setup-environment))
;let
(define (let? exp) (tagged-list? exp 'let))
(define (let-defn exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (let-combination exp)
    (let ((defn (let-defn exp))
          (body (let-body exp)))
        (let ((vars (map car defn))
              (exps (map cadr defn)))
            (cons (make-lambda vars body)
                  exps))))
;amb
(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))
;
; (driver-loop)

;4.50
;感觉可以写一个exps的打乱函数，ramd->amb
;但是看到sicp-wiki上有人用的运行时随机，感觉后者更好,但是也很麻烦
;4.51
(define (permanent-set? exp)
    (tagged-list? exp 'permanent-set!))
(define (permanent-variable exp) (cadr exp))
(define (permanent-value exp) (caddr exp))
(define (analyze-permanent-set exp)
    (let ((var (permanent-variable exp))
          (vproc (analyze (permanent-value exp))))
        (vproc env
               (lambda (val fail2)
                    (let ((old-value (lookup-variable-value var env)))
                        (set-variable-value! var val env)
                        (succeed 'ok
                                 fail2)))
               fail)))
;4.52
(define (if-fail? exp) (tagged-list? exp 'if-fail))
(define (first-exp exp) (cadr exp))
(define (second-exp exp) (caddr exp))
(define (analyze-if-fail exp)
    (let ((first (analyze (first-exp exp)))
          (second (analyze (second-exp exp))))
        (lambda (env succeed fail)
            (first env
                   succeed
                   (lambda ()
                        (second env succeed fail))))))
;4.53
;prime-sum-pair的amb要比if-fail更深，前者的fail先执行，结果为所有prime序对
;4.54
(define (require? exp) (tagged-list? exp 'require))
(define (require-predicate exp) (cadr exp))
(define (analyze-require exp)
    (let ((pproc (analyze (require-predicate exp))))
        (lambda (env succeed fail)
            (pproc env
                   (lambda (val fail2)
                        (if (true? val)
                            (succeed 'ok fail2)
                            (fail2)))
                   fail))))
(driver-loop)
(define (require p) (if (not p) (amb)))