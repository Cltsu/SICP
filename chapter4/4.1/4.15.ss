;4.15
;停机问题，
;求值(try try)代表着让halts?判断(try try)会不会停机，
;如果halts?判断(try try)会停机，则会转入(run-forever)。
;不过此时是什么在(run-forever)?，是被halts判断为会停机的(try try)，
;被判为会停机的过程却进入了(run-forever)
;这里就产生了矛盾
;halts?判断不停机也会产生相似的矛盾。
;这表明了halts?是不存在的。