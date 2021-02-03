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
        (if (> (car l) p)
            (cons (car l) ((sup p) (cdr l)))
            ((sup p) (cdr l)))))) )

(define quicksort
  (lambda (l)
    (if (null? l ) l
         (append (quicksort ((inf (car l)) l)) (cons (car l) (quicksort ((sup (car l)) l)))))))

((sup 5) (list 2 3 1 5 7 3 4 5 9 1 2 10))

(quicksort (list 2 3 1 5 7 3 4 5 9 1 2 10))
(< 2 3)