;5.1
;ipad上
;5.2
(controller
    test-c 
        (test (op >) (reg c) (reg n))
        (branch (label fac-done))
        (assign p (op mul) (reg p) (reg c))
        (assign c (op inc) (reg c))
        (goto (label test-c))
    fac-done)
;5.3
(controller
    sqrt
        (assign x (read))
        (assign g (const 1.0))
    sqrt-iter
        (assign t (op square) (reg g))
        (assign t (op dec) (reg t) (reg x))
        (assign t (op abs) (reg t))
        (test (op <) (reg t) (const 0.001))
        (branch (label sqrt-done))
        
        (assign t (op div) (reg g) (reg x))
        (assign t (op avg) (reg g) (reg t))
        (assign g (reg t))
        (goto sqrt-iter)
    sqrt-done)
;5.4
;a)
(controller
        (assign continue (label expt-done))
        (assign b (read))
        (assign n (read))
    expt-loop
        (test (op =) (reg n) (const 1))
        (branch (label direct-1))
        (save continue)
        (assign continue (expt-after))
        (assign n (op -) (reg n) (const 1))
        (goto (label expt-loop))
    expt-after
        (restore continue)
        (assign val (op mul) (reg b) (reg val))
        (goto (reg continue))
    direct-1
        (assign val (const 1))
        (goto (reg continue))
    expt-done)
;b)
(controller
        (assign continue (label expt-done))
        (assign b (read))
        (assign n (read))
        (assign val (const 1))
    expt-loop
        (test (op =) (reg n) (const 1))
        (branch (label direct-ret))
        (save continue)
        (assign n (op -) (reg n) (const 1))
        (assign val (op *) (reg b) (reg vaL))
        (goto (label expt-loop))
    direct-ret
        (assign ret (reg val));也可以直接返回val
        (goto (lable expt-done))
    expt-done)  
;5.5
;pass
;5.6
;afterfib-n-1 下面的(restore continue) (save continue)