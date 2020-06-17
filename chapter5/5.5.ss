; (define (compile exp target linkage)
;     (cond ((self-evaluating? exp))))
(map (lambda (x) (display x) (newline))
(map (lambda (x) (list 'list x x)) '(operator empty-arglist no-operands? first-operand last-operand? adjoin-arg rest-operands
primitive-procedure? compound-procedure? apply-primitive-procedure procedure-parameters
procedure-environment  extend-environment procedure-body begin-actions first-exp last-exp?
rest-exps if-predicate true? if-alternative if-consequent assignment-variable assignment-value
set-variable-value! definition-variable definition-value define-variable! initialize-stack prompt-for-input
read get-global-environment announce-output user-print))
)
(exit)