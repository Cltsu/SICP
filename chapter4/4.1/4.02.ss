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
