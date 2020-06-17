;3.24
(define (assoc key records same-key?)
    (cond ((null? records) #f)
          ((same-key? key (caar records)) (car records))
          (else (assoc key (cdr records) same-key?))))
(define (make-table same-key?)
    (let ((local-table (list '*table*)))
        (define (lookup key-1 key-2)
            (let ((subtable (assoc key-1 (cdr local-table) same-key?)))
                (if subtable
                    (let ((record (assoc key-2 (cdr subtable) same-key?)))
                        (if record
                            (cdr record)
                            #f))
                    #f)))
        (define (insert! key-1 key-2 value)
            (let ((subtable (assoc key-1 (cdr local-table) same-key?)))
                (if subtable
                    (let ((record (assoc key-2 (cdr subtable) same-key?)))
                        (if record
                            (set-cdr! record value)
                            (set-cdr! subtable 
                                      (cons (cons key-2 value)
                                            (cdr subtable)))))
                    (set-cdr! local-table
                              (cons (list key-1 
                                          (cons key-2 value))
                                    (cdr local-table)))))
            'ok)
        (define (dispatch m)
            (cond ((eq? m 'lookup) lookup)
                  ((eq? m 'insert) insert!)
                  (else (error "unkown op -- TABLE" m))))
        dispatch))
;3.25
(define (make-table same-key?)
    (let ((local-table (list 'table)))
        (define (lookup keys curtable)
            (if (null? keys) #f
                (let ((subtable (assoc (car keys) (cdr curtable) same-key?)))
                    (if subtable
                        (if (null? (cdr keys))
                            (cdr subtable)
                            (lookup (cdr keys) subtable))
                        #f))))
        (define (insert! keys value curtable)
            (if (null? keys) 
                'fail
                (let ((subtable (assoc (car keys) (cdr curtable) same-key?)))
                    (if subtable
                        (if (null? (cdr keys))
                            (set-cdr! subtable value)
                            (insert! (cdr keys) value subtable))
                        (if (null? (cdr keys))
                            (set-cdr! curtable
                                      (cons (cons (car keys) value)
                                            (cdr curtable)))
                            (let ((newsub (list (car keys))))
                                (set-cdr! curtable
                                          (cons newsub
                                                (cdr curtable)))
                                (insert! (cdr keys) value newsub))))))
            'ok)
        (define (dispatch m)
            (cond ((eq? m 'lookup) (lambda (keys) (lookup keys local-table)))
                  ((eq? m 'insert) (lambda (keys value) (insert! keys value local-table)))
                  ((eq? m 'show) local-table)
                  (else (error "unkown op -- TABLE" m))))
        dispatch))
;3.26 不想做
;3.27
;不能
(define (show . s)
    (map (lambda (x) (begin (display x) (newline) x)) s))
(define test (make-table equal?))
((test 'insert) (list 'a 'b) 1)
((test 'insert) (list 'a 'c) 2)
((test 'insert) (list 'd 'q) 1)
(display ((test 'lookup) (list 'a 'c)))
(display (test 'show))
(exit)