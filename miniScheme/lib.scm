(definerec fact 
  (lambda (n) 
    (if (= n 0) 1 (* n (fact (- n 1))))))

(define not
  (lambda (b) (if b #f #t)))

(define caar (lambda (l) (car (car l))))

(define cdar (lambda (l) (cdr (car l))))

(define pair? (lambda(n) (not (atom? n))))

(definerec lookup 
  (lambda (id)
    (lambda (env) 
       (if (null? env) 
            (print "variablenotbound")
            (if (equal? id (caar env)) 
                 (cdar env)
                 ((lookup id) (cdr env))))))) 

(define env (list ( list (quote y) 3)
                  ( list (quote x) 2))) 

(definerec append
  (lambda (l1)
    (lambda (l2)
      (if (null? l1) l2
          (cons (car l1) ((append (cdr l1)) l2))))))

(definerec inf
  (lambda (p)
    (lambda (l)
      (if (null? l) l
        (if (< (car l) p)
            (cons (car l) ((inf p) (cdr l)))
            ((inf p) (cdr l)))))))

(definerec sup
  (lambda (p)
    (lambda (l)
      (if (null? l) l
        (if (>= (car l) p)
            (cons (car l) ((sup p) (cdr l)))
            ((sup p) (cdr l)))))))

(begin (print "et maintenant le quicksort") (eol))

(definerec quicksort
  (lambda (l)
    (if (or (null? l ) (null? (cdr l))) l
         ((append (quicksort ((inf (car l)) (cdr l)))) (cons (car l) (quicksort ((sup (car l)) (cdr l))))))))

(begin (print "quelques test") (eol))

((append (list 1 2 3)) (list 4 5 6))

(quicksort (list 34 3 1 5 7 3 4 5 9 1 2 10 123 7 49 59 39 50 20 57 4 50 39 58 9 34 54 35 34 59 24 593 4 10 14))

((lookup (quote x)) env)

(begin (print "fin de la librairie") (eol))