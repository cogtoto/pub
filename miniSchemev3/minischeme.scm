; minischeme

(define env (list (cons 'a 1976)
                  (cons '+ (lambda (x y ) (+ x y)))
                  ))

(define modifierenv!
  (lambda (id newvalue env)
    (if (pair? env)
         (if (eq? (caar env) id)
             (begin (set-cdr! (car env) value) value)
             (modifierenv! id newvalue (cdr env)))
         "not found")))


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
  (lambda (liaisons lcorps env)
         (let ((lvar (mapcar liaisons))
               (lexp (mapcadr liaisons)))
            (eprogn lcorps (extend env lvar (evlis lexp env))))))

(define insertenv!
  (lambda (id newvalue env)
          (begin (set-cdr! env (cons (car env)(cdr env)))
                 (set-car! env (cons id newvalue)))))

(define evlis
  (lambda (exps env)
    (if (not (null? exps))
        (cons (evaluer (car exps) env)
              (evlis (cdr exps) env))
        '() )))

(define evaluer
  (lambda ( e env)
    (cond ((symbol? e) (lookup e env))
          ((or (number? e) (boolean? e)) e)
          ((formespeciale? e) (evaluerformespeciale e env))
          (else (invoke (evaluer (car e) env) (evlis (cdr e) env) env)))))

(define evaluerformespeciale 
     (lambda (e env)
               (if (equal? (car e) 'quote)
                    (cadr e)
                    (if (equal? (car e) 'if)
                        (if (evaluer (cadr e) env)
                            (evaluer (caddr e) env)
                            (evaluer (cadddr e) env))
                        (if (equal? (car e) 'begin)
                            (eprogn (cdr e) env)
                              (if (equal? (car e) 'let)
                                (evallet (cadr e) (cddr e) env)
                                (if (equal? (car e) 'define)
                                    (insertenv! (cadr e) (evaluer (caddr e) env) env)
                                    (if (equal? (car e) 'lambda)
                                        (list 'fermeture  e  env )
                                        (if (equal? (car e) 'set-cdr!)
                                          (set-cdr! (evaluer (cadr e) env) (evaluer (caddr e) env) )
                                          (if (equal? (car e) 'set-car!)
                                          (set-car! (evaluer (cadr e) env) (evaluer (caddr e) env) )
                                        "erreur evaluerformespeciale" ))))))))))

(define invoke
  (lambda (fn larg env)
        (if (and (pair? fn) (equal? 'fermeture (car fn))) 
           (let ((lpar (cadr (cadr fn)))
                 (lcorps (cddr (cadr fn)))
                 (envf (caddr fn)))
              (eprogn lcorps (extend envf lpar larg)))
           (apply fn larg))))

(define formespeciale?
  (lambda (e)
    (and (pair? e) (or (equal? (car e) 'quote)
                       (equal? (car e) 'if)
                       (equal? (car e) 'lambda)
                       (equal? (car e) 'let)
                       (equal? (car e) 'begin)
                       (equal? (car e) 'define)
                       (equal? (car e) 'set!)
                       (equal? (car e) 'set-cdr!)
                       (equal? (car e) 'set-car!)
                       (equal? (car e) 'letrec)
                       ))))

 (define extend
  (lambda (env variables values)
        (if (or (null? variables) (null? values))
            env
            (cons (cons (car variables) (car values))
                  (extend env (cdr variables) (cdr values))))))

(define eprogn
  (lambda (exps env)
     (if (not (null? exps))
          (if (not (null? (cdr exps)))
              (begin (evaluer (car exps) env)
                     (eprogn (cdr exps) env) )
              (evaluer (car exps) env) )
         '()) ) )

(define lookup 
  (lambda (id env)
      (if (null? env) 
            #f
            (if (equal? id (caar env)) 
                 (cdar env)
                 (lookup id (cdr env))))))

(evaluer 'a env)
(evaluer 5 env)
(evaluer '(+ 5 7) env)
(evaluer '(define f (lambda (x) x)) env)
(evaluer '((lambda (c) c) 5) env)
(evaluer '(f 9) env)


(evaluer  
'(let ((a 1))
  (let ((f (lambda (b) (+ b a)))) 
     (let ((a 3)) (f 10)))
) env)




(define top-level 
 (lambda ()
   (let ((a (read)))
      (if (equal? a 'quit)
          (display "good bye") 
          (begin 
            (display (evaluer a env))
            (newline)
            (top-level)
            )))))