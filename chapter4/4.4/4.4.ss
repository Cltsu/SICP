(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query output:")

(define (query-driver-loop)
    (prompt-for-input input-prompt)
    (let ((q (query-syntax-process (read))))
        (cond ((assertion-to-be-added? q)
               (add-rule-or-assertion! (add-assertion-body q))
               (newline)
               (display "Assertion added to database.")
               (query-driver-loop))
              (else
               (newline)
               (display output-prompt)
               (display-stream
                (stream-map
                    (lambda (frame)
                        (instantiate q
                                     frame
                                     (lambda (v f)
                                        (contract-question-mark v))))
                    (qeval q (singleton-stream '()))))
               (query-driver-loop)))))
(define (instantiate exp frame unbound-var-handler)
    (define (copy exp)
        (cond ((var? exp)
               (let ((binding (binding-in-frame exp frame)))
                (if binding
                    (copy (binding-value binding))
                    (unbound-var-handler exp frame))))
              ((pair? exp)
                (cons (copy (car exp)) (copy (cdr exp))))
              (else exp)))
    (copy exp))
;4.4.4.2 求值器
(define (qeval query frame-stream)
    (let ((qproc (get (type query) 'qeval)))
        (if qporc
            (qproc (contents query) frame-stream)
            (simple-query query frame-stream))))
(define (simple-query query-pattern frame-stream)
    (stream-flatmap
        (lambda (frame)
            (stream-append-delayed
                (find-assertions query-pattern frame)
                (delay (apply-rules query-pattern frame))))
        frame-stream))
(define (conjoin conjuncts frame-stream)
    (if (empty-conjunction? conjuncts)
        frame-stream
        (conjoin (rest-conjuncts conjuncts)
                 (qeval (first-conjunct conjuncts)
                        frame-stream))))
(put 'and 'qeval conjoin)
;
(define (disjoin disjuncts frame-stream)
    (if (empty-disjunction? disjuncts)
        the-empty-stream
        (interleave-delayed
            (qeval (first-disjunct disjuncts) frame-stream)
            (delay (disjoin (rest-disjuncts disjuncts)
                            frame-stream)))))
(put 'or 'qeval disjoin)
;
(define (negate operands frame-stream)
    (stream-flatmap
        (lambda (frame)
            (if (stream-null? (qeval (negate-query operands)
                                     (singleton-stream frame)))
                (singleton-stream frame)
                the-empty-stream))
        frame-stream))
(put 'not 'qeval negate)
;
(define (lisp-value call frame-stream)
    (stream-flatmap
        (lambda (frame)
            (if (execute
                    (instantiate
                        call
                        frame
                        (lambda (v f)
                            (error "lisp-value" "unknown pat var" v))))
                (singleton-stream frame)
                the-empty-stream))
        frame-stream))
(put 'lisp-value 'qeval lisp-value)
(define (execute exp)
    (apply (eval (predicate exp) user-initial-environment)
           (args exp)))
;
(define (always-true ignore frame-stream) frame-stream)
(put 'always-true 'qeval always-true)

;4.4.4.3 模式匹配和断言
;以一个模式和一个框架输入，返回框架的流
(define (find-assertions pattern frame)
    (stream-flatmap (lambda (datum)
                        (check-an-assertion datum pattern frame))
                    (fetch-assertions pattern frame)))

;以一个断言、一个模式和一个框架作为输入，返回空框架或者扩展之后的框架
(define (check-an-assertion assertion query-pat query-frame)
    (let ((match-result (pattern-match query-pat assertion query-frame)))
        (if (eq? match-result 'failed)
            the-empty-stream
            (singleton-stream match-result))))

;一个模式或其一部分、一个断言或其一部分、一个框架 => 扩充之后的框架或者failed标识
(define (pattern-match pat dat frame)
    (cond ((eq? frame 'failed) 'failed)
          ((equal? pat dat) frame)
          ((var? pat) (extend-if-consistent pat dat frame))
          ((and (pair? pat) (pair? dat))
            (pattern-match (cdr pat)
                           (cdr dat)
                           (pattern-match (car pat)
                                          (car bat)
                                          frame)))
          (else 'failed)))

;一个变量、断言中的一个常量、一个框架 => 扩充约束之后的框架
(define (extend-if-consistent var dat frame)
    (let ((binding (binding-in-frame var frame)))
        (if binding
            (pattern-match (binding-value binding) dat frame)
            (extend var dat frame))))

;4.4.4.4规则和合一
;一个模式、一个框架 => 通过规则而扩充的框架流
;map的结果可能是failed，所以用flatmap，将所有结果包装为流再flat。
(define (apply-rules pattern frame)
    (stream-flatmap (lambda (rule)
                        (apply-a-rule rule pattern frame))
                    (fetch-rules pattern frame)))

;一个规则、一个模式、一个框架 => 扩充之后的一个框架的流，或者空流
(define (apply-a-rule rule query-pattern query-frame)
    (let ((clean-rule (rename-variables-in rule)))
        (let ((unify-result 
                    (unify-match query-pattern
                                 (conclusion clean-rule)
                                 query-frame)))
            (if (eq? unify-result 'failed)
                the-empty-stream
                (qeval (rule-body clean-rule)
                       (singleton-stream unify-result))))))

;重命名rule变量
(define (rename-variables-in rule)
    (let ((rule-application-id (new-rule-application-id)))
        (define (tree-walk exp)
            (cond ((var? exp)
                   (make-new-variable exp rule-application-id))
                  ((pair? exp)
                    (cons (tree-walk (car exp))
                          (tree-walk (cdr exp))))
                  (else exp)))
        (tree-walk rule)))

;合一算法: 两个模式、一个框架 => 一个扩充的框架或者failed
(define (unify-match p1 p2 frame)
    (cond ((eq? frame 'failed) 'failed)
          ((equal? p1 p2) frame)
          ((var? p1) (extend-if-possible p1 p2 frame))
          ((var? p2) (extend-if-possible p2 p1 frame))
          ((and (pair? p1) (pair? p2))
            (unify-match (cdr p1)
                         (cdr p2)
                         (unify-match (car p1)
                                      (car p2)
                                      frame)))
          (else 'failed)))

;合一 扩展frame
(define (extend-if-possible var val frame)
    (let ((binding (binding-in-frame var frame)))
        (cond (binding
                (unify-match
                    (binding-value binding) val frame))
              ((var? val);;;*
                (let ((binding (binding-in-frame val frame)))
                    (if binding
                        (unify-match
                            var (binding-value binding) frame)
                        (extend var val frame))))
              ((depends-on? val var frame);;;*
                'failed)
              (else (extend var val frame)))))

;
(define (depends-on? exp var frame)
    (define (tree-walk e)
        (cond ((var? e)
               (if (equal? var e)
                   #t
                   (let ((b (binding-in-frame e frame)))
                    (if b
                        (tree-walk (binding-value b))
                        #f))))
              ((pair? e)
                (or (tree-walk (car e))
                    (tree-walk (cdr e))))
              (else #f)))
    (tree-walk exp))

;4.4.4.5 数据库的维护
;assertion
(define THE-ASSERTIONS the-empty-stream)
;模式开头是符号常量symbol即可查找索引
(define (fetch-assertions pattern frame)
    (if (use-index? pattern)
        (get-indexed-assertions pattern)
        (get-all-assertions)))
(define (get-all-assertions pattern) THE-ASSERTIONS)
(define (get-indexed-assertions pattern)
    (get-stream (index-key-of pattern) 'assertion-stream))
(define (get-stream key1 key2)
    (let ((s (get key1 key2)))
        (if s s the-empty-stream)))
;rule
;rule的结论部分可以以变量开头，则将变量开头的rule全部存入索引为?的表格中
(define THE-RULES the-empty-stream)
(define (fetch-rules pattern frame)
    (if (use-index? pattern)
        (get-indexed-rules pattern)
        (get-all-rules)))
(define (get-all-rules) THE-RULES)
(define (get-indexed-rules pattern)
    (stream-append
        (get-stream (index-key-of pattern) 'rule-stream)
        (get-stream '? 'rule-stream)))
;
(define (add-rule-or-assertion! assertion)
    (if (rule? assertion)
        (add-rule! assertion)
        (add-assertion! assertion)))
(define (add-assertion! assertion)
    (store-assertion-in-index assertion);存进index
    (let ((old-assertion THE-ASSERTIONS));存进总断言流
        (set! THE-ASSERTIONS
              (cons-stream assertion old-assertions))
        'ok))
(define (add-rule! rule)
    (store-rule-in-index rule)
    (let ((old-rules THE-RULES))
        (set! THE-RULES (cons-stream rule old-rules))
        'ok))
(define (store-assertion-in-index assertion)
    (if (indexable? assertion);显然断言没有变量，只需要保证car不是其他数据结构，如list，即可。
        (let ((key (index-key-of assertion)))
            (let ((current-assertion-stream
                    (get-stream key 'assertion-stream)))
                (put key
                     'assertion-stream
                     (cons-stream assertion
                                  (current-assertion-stream)))))))
(define (store-rule-in-index rule)
    (let ((pattern (conclusion rule)))
        (if (indexable pattern)
            (let ((key (index-key-of pattern)))
                (let ((current-rule-stream
                        (get-stream key 'rule-stream)))
                    (put key
                         'rule-stream
                         (cons-stream rule current-rule-stream)))))))
(define (indexable? pat);开头为symbol或者变量即可索引
    (or (constant-symbol? (car pat))
        (var? (car pat))))
(define (index-key-of pat)
    (let ((key (car pat)))
        (if (var? key) '? key)))
(define (use-index? pat)
    (constant-symbol? (car pat)))

;4.4.4.6流操作
(define (stream-append-delayed s1 delayed-s2)
    (if (stream-null? s1)
        (force delayed-s2)
        (cons-stream
            (stream-car s1)
            (stream-append-delayed (stream-cdr s1) delayed-s2))))
(define (interleave-delayed s1 delayed-s2)
    (if (stream-null? s1)
        (force delayed-s2)
        (cons-stream
            (stream-car s1)
            (interleave-delayed (force delayed-s2)
                                (delay (stream-cdr s1))))))
;将二维的流转换为一维
(define (stream-flatmap proc s)
    (flatten-stream (stream-map proc s)))
(define (flatten-stream stream)
    (if (stream-null? stream)
        the-empty-stream
        (interleave-delayed
            (stream-car stream)
            (delay (flatten-stream (stream-cdr stream))))))
(define (singleton-stream x)
    (cons-stream x the-empty-stream))
;4.4.4.7 查询的语法过程
(define (type exp)
    (if (pair? exp)
        (car exp)
        (error "type" "Unknown expreesion TYPE" exp)))
(define (contents exp)
    (if (pair? exp)
        (cdr exp)
        (error "contents" "Unknown expreesion CONTENTS" exp)))
(define (assertion-to-be-added? exp)
    (eq? (type exp) 'assert!))
(define (add-assertion-body exp)
    (car (contents exp)))

(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cde exps))

(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjunct exps) (cdr exps))

(define (negate-query exps) (car exps))
(define (args exps) (cdr exps))

(define (rule? statement)
    (tagged-list? statement 'rule))
(define (conclusion rule) (cadr rule))
(define (rule-body rule)
    (if (null? (cddr rule))
        '(always-true)
        (caddr rule)))

;将所有?x换成(? x)
(define (query-syntax-process exp)
    (map-over-symbols expand-question-mark exp))
(define (map-over-symbols proc exp)
    (cond ((pair? exp)
           (cons (map-over-symbols proc (car exp))
                 (map-over-symbols proc (cdr exp ))))
          ((symbol? exp) (proc exp))
          (else exp)))
(define (expand-question-mark symbol)
    (let ((chars (symbol->string symbol)))
        (if (string=? (substring chars 0 1) "?")
            (list '? 
                  (string->symbol
                    (substring chars 1 (string-length chars))))
            symbol)))
;经过上面的处理之后，所有的var变成了'?为首的list
(define (var? exp)
    (tagged-list exp '?))
(define (constant-symbol? exp) (symbol? exp))
;给每个变量修改名字
(define rule-counter 0)
(define (new-rule-application-id)
    (set! rule-counter (+ 1 rule-counter))
    rule-counter)
(define (make-new-variable var rule-application-id)
    (cons '? (cons rule-application-id (cdr var))))
;将仍未约束的变量变回去
(define (contract-question-mark variable)
    (string->symbol
        (string-append "?"
            (if (number? (cadr variable))
                (string-append (symbol->string (caddr variable))
                               "-")
                (symbol->string (cadr variable))))))
;4.4.4.8 框架和约束
(define (make-binding variable value)
    (cons variable value))
(define (binding-variable binding) (car binding))
(define (binding-value binding) (cdr binding))
(define (binding-in-frame variable value)
    (assoc variable value))
(define (extend variable value frame)
    (cons (make-binding variable value) frame))
interleave

;4.72
;3.5使用interleave是防止无穷流引发的display的问题，这一章中不存在无穷流是可以不使用interleave的
;4.73
;因为flatten-map自身不产生con-stream，所以它无法将interleave的第二个参数隐式地delay
;如果是一个infinite stream，会进入死循环,如果是finite stream，它可以work，但是这样很蠢，而且失去stream的意义。
;4.74
(define (simple-stream-flatmap proc s)
    (simple-flatten (stream-map proc s)))
(define (simple-flatten stream)
    (stream-map (lambda (x) (car x))
                (stream-filter (lambda (x) (not (null-stream? x))))
                               stream))
;b)
;不会改变
;4.75
(define (unique-body exp) (car exp))
(define (uniquely-asserted query frame-stream)
    (stream-flatmap
        (lambda (frame)
            (let ((s (qeval (unique-body query)
                            (singleton-stream frame))))
                (if (or (null-stream? s)
                        (not (null-stream? (stream-cdr s))))
                    the-empty-stream
                    s)))
        frame-stream))
(put 'unique 'qeval uniquely-asserted)
;4.76
;not finish
(define (conjoin conjuncts frame-stream)
    (if (empty-conjunction? conjuncts)
        frame-stream
        (merge (qeval (first-conjunct conjuncts)
                      frame-stream)
               (conjoin (rest-conjunct conjuncts)
                        frame-stream)))) 
;4.77