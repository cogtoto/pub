(define y (lambda (h)
    (lambda (x)
      (((lambda (f) (lambda (x) ((f f) x))) 
        (lambda (f)
          ((lambda (g)
             (g (lambda (x) ((f f) x))))
           h)))
       x))))

(definerec fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))

(define not  (lambda (b) (if b #f #t)))
(define caar (lambda (l) (car (car l))))
(define cadr (lambda (l) (car (cdr l))))
(define caddr (lambda (l) (car (cdr (cdr l)))))
(define cadddr (lambda (l) (car (cdr (cdr (cdr l))))))
(define cddr (lambda (l) (cdr (cdr l))))
(define cdddr (lambda (l) (cdr (cdr (cdr l)))))
(define cdar (lambda (l) (cdr (car l))))
(define atom? (lambda (n) (or (number? n) (string? n) (boolean? n) (symbol? n))))
(define pair? (lambda(n) (not (atom? n))))

(definerec append
  (lambda (l1)
    (lambda (l2)
      (if (null? l1) l2
          (cons (car l1) ((append (cdr l1)) l2))))))

(define apply 
   (lambda (fn arglist)
     (cons fn arglist))) 

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

(definerec quicksort
  (lambda (l)
    (if (or (null? l ) (null? (cdr l))) l
         ((append (quicksort ((inf (car l)) (cdr l)))) (cons (car l) (quicksort ((sup (car l)) (cdr l))))))))

(begin (print "fin de la librairie") (eol))
(begin (print "interprete lisp") (eol))

(define env (list
      (cons 'x 3)
      (cons 'not (lambda (b) (if b #f #t)))
      (cons '= (lambda (x y) (= x y)))
      (cons '*   (lambda (x y) (* x y)))
      (cons '-   (lambda (x y) (- x y)))
      (cons '+   (lambda (x y) (+ x y)))
      (cons 'atom? (lambda (x) (atom? x)))
      (cons 'boolean? (lambda (x) (boolean? x)))
      (cons 'number? (lambda (x) (number? x)))
      (cons 'cons (lambda (x y) (cons x y)))
      (cons 'car (lambda (x) (car x)))
      (cons 'cdr (lambda (x) (cdr x)))
      (cons 'caar (lambda (l) (car (car l))))
      (cons 'cadr (lambda (l) (car (cdr l))))
      (cons 'caddr (lambda (l) (car (cdr (cdr l)))))
      (cons 'cadddr (lambda (l) (car (cdr (cdr (cdr l))))))
      (cons 'cddr (lambda (l) (cdr (cdr l))))
      (cons 'cdddr (lambda (l) (cdr (cdr (cdr l)))))
      (cons 'cdar (lambda (l) (cdr (car l))))
            
      (cons 'pair? (lambda(n) (not (atom? n))))

      (cons 'apply    (lambda (fn arglist) ((evaluate (cons fn arglist)) env))) 
      (cons 'y    (lambda (h)
                          (lambda (x)
                            (((lambda (f) (lambda (x) ((f f) x))) 
                              (lambda (f)
                                ((lambda (g)
                                    (g (lambda (x) ((f f) x))))
                                  h)))
                              x))))

      (cons 'fact   (lambda (any) ((y 
                      (lambda (f)
                        (lambda (n)
                          (if (= n 0) 1
                            (* n (f (- n 1))))))) 
                      any)) )
                                        
      (cons 'lookup  (lambda (any) ((y
                      (lambda (f)
                        (lambda (id)
                          (lambda (env) 
                            (if (null? env) 
                                  (print "lisp variable not bound")
                                  (if (equal? id (caar env)) 
                                      (cdar env)
                                      ((f id) (cdr env))))))))  
                        any)) )
      (cons 'eprogn  (lambda (any) ((y
                      (lambda (f)
                        (lambda (exps)
                          (lambda (env)
                            (if (not (null? exps))
                                (if (not (null? (cdr exps)))
                                    (begin ((evaluate (car exps)) env)
                                          ((f (cdr exps)) env) )
                                    ((evaluate (car exps)) env) )
                                    '()) ) )) )  
                      any)) )
      (cons 'evlis  (lambda (any) ((y
                      (lambda (f)
                        (lambda (exps)
                          (lambda (env)
                            (if (not (null? exps))
                              (cons ((evaluate (car exps)) env)
                              ((evlis (cdr exps)) env))
                              '() )))))
                      any)) )
    
      (cons 'invoke  
              (lambda (fn)
                (lambda (larg)
                  (lambda (env)
                    (if (and (pair? fn) (equal? 'fctuser (car fn))) 
                      (let ((lpar (car (cdr (cdr fn))))
                            (lcorps (cdddr fn)))
                          ((eprogn lcorps) (((extend env) lpar) larg)))
                      (apply fn larg))))) )
              
    (cons 'extend (lambda (any) ((y
                      (lambda (f)
                        (lambda (env)
                          (lambda (variables)
                            (lambda (values)
                              (if (null? variables)
                                  env
                                  (cons (cons (car variables) (car values))
                                        (((f env) (cdr variables)) (cdr values)))))))) )
                    any)) )

    (cons 'mapcar (lambda (any) ((y
                      (lambda (f)
                        (lambda (l)
                            (if (null? l)
                              l
                              (cons (car (car l)) (f (cdr l)))))))
                        any)) )

    (cons 'mapcadr (lambda (any) ((y
            (lambda (f)
              (lambda (l)
                  (if (null? l)
                    l
                    (cons (cadr (car l)) (f (cdr l)))))))
              any)) )
    (cons 'evallet 
      (lambda (liaisons)
          (lambda (lcorps)
            (lambda (env)
              (let ((lvar (mapcar liaisons))
                    (lexp (mapcadr liaisons)))
                  ((eprogn lcorps) (((extend env) lvar) ((evlis lexp) env))))))))

(cons 'evaluate (lambda (any) ((y
                                  (lambda (f)
            (lambda (e)
              (lambda(env)
                (if (symbol? e)
                    ((lookup e) env)
                    (if (or (number? e) (boolean? e))
                        e
                        (if (equal? (car e) 'quote)
                            (cadr e)
                            (if (equal? (car e) 'if)
                                (if ((f (cadr e)) env)
                                    ((f (caddr e)) env)
                                    ((f (cadddr e)) env))
                                (if (equal? (car e) 'begin)
                                    ((eprogn (cdr e)) env)
                                      (if (equal? (car e) 'let)
                                        (((evallet (cadr e)) (cddr e)) env)
                                        (if (equal? (car e) 'lambda)
                                            (cons 'fctuser e)
                                            (((invoke ((f (car e)) env))
                                                        ((evlis (cdr e)) env)) env) ) ) ) ) ) ) ) ) ) ) )
                    any)) )
           
              ) ) 

(define env2 (cons (cons 'env env) env))
(define env env2)

(definerec lookup 
  (lambda (id)
    (lambda (env) 
       (if (null? env) 
            (print "variable not bound")
            (if (equal? id (caar env)) 
                 (cdar env)
                 ((lookup id) (cdr env))))))) 

(definerec eprogn
  (lambda (exps)
    (lambda (env)
      (if (not (null? exps))
          (if (not (null? (cdr exps)))
              (begin ((evaluate (car exps)) env)
                     ((eprogn (cdr exps)) env) )
              ((evaluate (car exps)) env) )
         '()) ) ))

 (definerec evlis
  (lambda (exps)
    (lambda (env)
      (if (not (null? exps))
          (cons ((evaluate (car exps)) env)
                ((evlis (cdr exps)) env))
          '() ))))

(define invoke
  (lambda (fn)
    (lambda (larg)
      (lambda (env)
        (if (and (pair? fn) (equal? 'fctuser (car fn))) 
           (let ((lpar (car (cdr (cdr fn))))
                 (lcorps (cdddr fn)))
              ((eprogn lcorps) (((extend env) lpar) larg)))
           (apply fn larg))))))

(definerec extend
  (lambda (env)
    (lambda (variables)
      (lambda (values)
        (if (null? variables)
            env
            (cons (cons (car variables) (car values))
                  (((extend env) (cdr variables)) (cdr values))))))))

(definerec map
 (lambda (f)
   (lambda (l)
    (if (null? l)
         l
         (cons (f (car l)) ((map  f) (cdr l)))))))

(definerec mapcar
   (lambda (l)
      (if (null? l)
         l
         (cons (car (car l)) (mapcar (cdr l))))))

(definerec mapcadr
   (lambda (l)
      (if (null? l)
         l
         (cons (cadr (car l)) (mapcadr (cdr l))))))
      
(define evallet
  (lambda (liaisons)
    (lambda (lcorps)
      (lambda (env)
         (let ((lvar (mapcar liaisons))
               (lexp (mapcadr liaisons)))
            ((eprogn lcorps) (((extend env) lvar) ((evlis lexp) env))))))))

(definerec evaluate 
  (lambda (e)
    (lambda(env)
      (if (symbol? e)
          ((lookup e) env)
          (if (or (number? e) (boolean? e))
              e
              (if (equal? (car e) 'quote)
                  (cadr e)
                  (if (equal? (car e) 'if)
                      (if ((evaluate (cadr e)) env)
                          ((evaluate (caddr e)) env)
                          ((evaluate (cadddr e)) env))
                      (if (equal? (car e) 'begin)
                          ((eprogn (cdr e)) env)
                            (if (equal? (car e) 'let)
                              (((evallet (cadr e)) (cddr e)) env)
                              (if (equal? (car e) 'lambda)
                                   (cons 'fctuser e)
                                   (((invoke ((evaluate (car e)) env))
                                               ((evlis (cdr e)) env)) env) ) ) ) ) ) ) ) ) ) ) 
                         
((evaluate 'x) env)

((evaluate '(= 7 7)) env)

((evaluate '((lambda (x y) (+ x y)) 5 6)) env)

((evaluate '((lambda (x y) (+ x y)) 4 5)) env)

((evaluate '(let ((a (+ 3 2)) (b 5)) (+ a b))) env) 

((evaluate '((lookup (quote x)) env)) env) 

((evaluate '(quote x)) env)

((evaluate '(fact 5)) env)

((evaluate '((evaluate (quote (let ((a (+ 3 2)) (b 5)) (+ a b)))) env)) env)

((evaluate '((evaluate (quote ((lambda (x y) (+ x y)) 5 6) )) env)) env)

((evaluate '((evaluate (quote 
(fact 5)
)) env)) env)

((evaluate '((evaluate (quote 
(let ((a 1))
  (let ((f (lambda (b) (+ b a)))) 
     (let ((a 3)) (f 10))))
)) env)) env)

((evaluate '((evaluate (quote ((evaluate (quote (
  (lambda (x y) (+ x y)) (fact 5) (fact 6) )))
             env))) env)) env)


((evaluate (quote ((evaluate (quote ((evaluate (quote ((evaluate (quote (
  (lambda (x y) (+ x y)) (fact 5) (fact 6) )))
             env))) env))) env))) env)