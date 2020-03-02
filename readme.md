# SICP
部分SICP习题解答，仅供参考   
### 注意事项
- 数据导向相关的习题均未测试
- 3.5节cons-stream和delay的实现需要使用宏
```
(define-syntax delay
  (syntax-rules ()
    ((_ exp) (memo-proc (lambda () exp)))))
(define (force delay-proc)
    (delay-proc))
(define-syntax cons-stream
  (syntax-rules ()
    [(cons-stream x y) (cons x (delay y))]))
```
