(define lookup 
  (lambda (id)
    (lambda (env) 
       (if (null? env) 
            (print "variablenotbound")
            (if (equal? id (caar env)) 
                 (cdar env)
                 ((lookup id) (cdr env))))))) 

(define env (list ( list (quote y) 3)
                  ( list (quote x) 2))) 

((lookup (quote x)) env)

(define append2
  (lambda (l1)
    (lambda (l2)
      (if (null? l1) l2
          (cons (car l1) ((append2 (cdr l1)) l2))))))

((append2 (list 1 2 3)) (list 4 5 6))

(define inf
  (lambda (p)
    (lambda (l)
      (if (null? l) l
        (if (< (car l) p)
            (cons (car l) ((inf p) (cdr l)))
            ((inf p) (cdr l)))))) )

(define sup
  (lambda (p)
    (lambda (l)
      (if (null? l) l
        (if (>= (car l) p)
            (cons (car l) ((sup p) (cdr l)))
            ((sup p) (cdr l)))))) )

(define quicksort
  (lambda (l)
    (if (or (null? l ) (null? (cdr l))) l
         (append (quicksort ((inf (car l)) (cdr l))) (cons (car l) (quicksort ((sup (car l)) (cdr l))))))))

((sup 5) (list 2 3 1 5 7 3 4 5 9 1 2 10))

(quicksort (list 2 3 1 5 7 3 4 5 9 1 2 10 123 7 49 59 39 50 20 57 4 50 39 58 9 34 54 35 34 59 24 593 4 10 14))
(< 2 3)


(define quicksort2
  (lambda (l)
    (if (or (null? l ) (null? (cdr l))) l
        ((append2 (quicksort2 ((inf (car l)) (cdr l)))) (cons (car l) (quicksort2 ((sup (car l)) (cdr l))))))))

(quicksort2 (list 2 3 4 2 1))
