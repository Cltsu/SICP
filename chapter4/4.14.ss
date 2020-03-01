;4.14
;我们能使用primitive过程，是因为我们直接使用了部分底层的scheme的数据结构或者定义，比如car，#t。
;我们可以将布尔值设为'true-mod,'false-mod，但是我们就无法将scheme的 = 符号作为primitive过程引入，因为等号返回的是#t\#f(chez scheme)。
;现在回到原题，我们将用底层scheme和新scheme区分两个层次
;关键在于map的参数能否被底层scheme的apply识别，map的第一个参数为函数
;但不是底层scheme的函数，而是我们实现的新scheme的函数,而底层scheme的apply则无法识别新scheme里的函数
