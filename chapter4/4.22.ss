;4.22
;let只是语法糖，所以实现let不需要额外的analyze-let
;直接在analyze函数中加上
;((let? exp) (analyze (let->combination exp)))