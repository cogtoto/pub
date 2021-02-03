(define lookup 
  (lambda (id)
    (lambda (env) 
      (if (null? env) 
          "variable not bound"
          (if (equal? id (caar env)) 
              (cdar env)
              ((lookup id) (cdr env))))))) 


(define map2
 (lambda (f)
   (lambda (l)
    (if (null? l)
         l
         (cons (f (car l)) ((map2  f) (cdr l)))))))

(define evallet
  (lambda (liaisons)
    (lambda (lcorps)
      (lambda (env)
         (let ((lvar ((map2 car) liaisons))
               (lexp ((map2 cadr) liaisons)))
            ((eprogn lcorps) (((extend env) lvar) ((evlis lexp) env))))))))

; ((a 2) (b 3))

(define mapcar
   (lambda (l)
      (if (null? l)
         l
         (cons (car (car l)) (mapcar (cdr l))))))

(define mapcadr
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


(define eprogn
  (lambda (exps)
    (lambda (env)
      (if (pair? exps)
          (if (not (null? (cdr exps)))
              (begin ((evaluate (car exps)) env)
                     ((eprogn (cdr exps)) env) )
              ((evaluate (car exps)) env) )
         '()) ) ))
                       
(define evlis
  (lambda (exps)
    (lambda (env)
      (if (not (null? exps))
          (cons ((evaluate (car exps)) env)
                ((evlis (cdr exps)) env))
          '() ))))

(define evaluate 
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
                          (if (equal? (car e) 'map)
                            ((map2 ((evaluate (cadr e)) env)) ((evaluate (caddr e)) env) ) 
                            (if (equal? (car e) 'let)
                              (((evallet (cadr e)) (cddr e)) env)
                              (if (equal? (car e) 'lambda)
                                   (cons 'fctuser e)
                                   (((invoke ((evaluate (car e)) env))
                                               ((evlis (cdr e)) env)) env) ) ) ) ) ) ) ) ) ) ) )
  
(define invoke
  (lambda (fn)
    (lambda (larg)
      (lambda (env)
        (if (and (pair? fn) (eq? 'fctuser (car fn))) ;fn = (fctuser (lambda lpar corps))
           (let ((lpar (car (cdr (cdr fn))))
                 (lcorps (cdddr fn)))
              ((eprogn lcorps) (((extend env) lpar) larg)))
           (apply fn larg))))))


(define extend
  (lambda (env)
    (lambda (variables)
      (lambda (values)
        (if (null? variables)
            env
            (cons (cons (car variables) (car values))
                  (((extend env) (cdr variables)) (cdr values))))))))

(define env (list
                  (cons 'x 3)
                  (cons 'id  (lambda (x) x))
                  (cons '= (lambda (x y) (= x y)))
                  (cons '*   (lambda (x y) (* x y)))
                  (cons '-   (lambda (x y) (- x y)))
                  (cons '+   (lambda (x y) (+ x y)))
                  (cons 'y    (lambda (h)
                                      (lambda (x)
                                        (((lambda (f) (lambda (x) ((f f) x))) 
                                          (lambda (f)
                                            ((lambda (g)
                                               (g (lambda (x) ((f f) x))))
                                             h)))
                                         x))))

                  (cons 'fact   (lambda (x) (((lambda (h)
                                      (lambda (x)
                                        (((lambda (f) (lambda (x) ((f f) x))) 
                                          (lambda (f)
                                            ((lambda (g)
                                               (g (lambda (x) ((f f) x))))
                                             h)))
                                         x))) (lambda (f)
                                  (lambda (n)
                                    (if (= n 0) 1
                                        (* n (f (- n 1))))))) x)))
                                                   
                  ) ) 

     
((evaluate 'x) env)

((evaluate '(begin (if #f x 999) x)) env)

((evaluate '(+ 2 3)) env)

((evaluate '((lambda (x y) (+ x y)) 4 5)) env)

((evaluate '(if (= 3 1) 2 3)) env)

((evaluate '((lambda (x) (if (= x 1) 2 3)) 5)) env)

((evaluate '(let ((a (+ 3 2)) (b 5)) (+ a b))) env)

((evaluate '(fact 5)) env)

((evaluate '((lambda (x y) (+ x y)) 4 5)) env)

((evaluate '(let ((f (lambda (x) (if (= x 1) 3 8)))) (f 5))) env)

;; ((evaluate '(let ((facto (lambda (n) (if (= n 0) 1 (* n (facto (- n 1))))))) (facto 5))) env)
 