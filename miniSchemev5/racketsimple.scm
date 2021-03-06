(define eval-constante
  (lambda (e r k)
    (k e)))

(define void
 (lambda () (cons '* '*)))

(define eval-quote
  (lambda (e r k)
    (k (cadr e))))

(define evaluate
  (lambda (e r k)
    ((if (not (pair? e))
          (if (or (number? e) (string? e) (boolean? e))
              eval-constante
              eval-variable)
          (if (equal? (car e) (quote quote))
              eval-quote
              (if (equal? (car e) (quote if))
               eval-if
               (if (equal? (car e) (quote define))
                   eval-assign
                   (if (equal? (car e) (quote lambda))
                       eval-abstraction
                       eval-application)))))
     e r k )))

(define eval-variable
  (lambda (e r k)
    (lookup e r
            (lambda (success-pair) (k (cdr success-pair)))
            (lambda () (wrong "symbole non lie" e)))))

(define eval-if
  (lambda (e r k)
    (evaluate (cadr e) r
              (lambda (v)
                (if v
                    (evaluate (caddr e) r k)
                    (evaluate (caddr e) r k))))))
                    
(define wrong
  (lambda (message object)
     (display "erreur")
     (display message)
     (display object)
     (newline)))

(define get-pair
  (lambda (id r success failure)
    (find-pair id r
               success
               (lambda ( )
                 (find-pair
                    id global-env success failure) )) ))

(define find-pair
  (lambda (elt alist success failure)
    ( (lambda (assq-result)
        (if assq-result
            (success assq-result)
            (failure)) )
      (assq elt alist) ) ) )

(define eval-assign
     (lambda (e r k)
       (evaluate (caddr e) r
                 (lambda (v)
                   (get-pair (cadr e) r
                             (lambda (success-pair)
                               (set-cdr! success-pair v)
                               (k (void)) )
                             (lambda ()
                               (set-cdr! global-env
                                         (cons (cons (cadr e) v)
                                               (cdr global-env))) 
                                                    (k (void))))))))
                 
(define lookup
  (lambda (id env k-success k-failure)
    (if (pair? env)
      (if (equal? (caar env) id)
          (k-success (car env))
          (lookup id (cdr env) k-success k-failure) )
      (k-failure))))     

(define update! 
  (lambda (id env value)
  (if (pair? env)
      (if (equal? (caar env) id)
          (set-cdr! (car env) value)
          (update! id (cdr env) value) )
      (set-cdr!  global-env (cons (cons id value) (cdr global-env)) ) ) ))

(define eval-define
  (lambda (e r k)
    (evaluate (caddr e) r
              (lambda (v) 
                (update! (cadr e) r v))))) 

(define eval-abstraction
  (lambda (e r k)
    (k (make-function (cadr e) (caddr e) r))))

(define make-function
  (lambda (varl corps r)
     (list 'cloture varl corps r)))

(define eval-application
  (lambda (e r k)
       (evaluate (car e) r
              (lambda (proc)
                (evlis (cdr e) r
                       (lambda (args)
                         (apply-procedure proc args k)))))))

(define evlis
  (lambda (e r k)
    (if (null? e)
        (k '())
        (evaluate (car e) r
                  (lambda (v)
                    (evlis (cdr e) r
                           (lambda (w)
                            (k (cons v w)))))))))

(define eprogn
  (lambda (e r k)
   (if (null? (cdr e))
       (evaluate (car e) r k)
       (evaluate (car e) r (lambda (v)
                             (eprogn (cdr e) r k))))))

 (define extend
  (lambda (env variables values)
        (if (or (null? variables) (null? values))
            env
            (cons (cons (car variables) (car values))
                  (extend env (cdr variables) (cdr values))))))
         
(define apply-procedure
  (lambda (proc args k)
    (if (equal? (car proc) 'cloture)
        (eprogn (list (caddr proc))
                (extend (cadddr proc) (cadr proc) args)
                k)
        (k (apply-primitive (cadr proc) args))))) 

(define apply-primitive
  (lambda (name args)
    (if (equal? name 'car)
        (car (car args))
    (if (equal? name 'cdr)
        (cdr (car args))
    (if (equal? name 'cons)
        (cons (car args) (cadr args))
    (if (equal? name 'set-car!)
        (set-car! (car args) (cadr args))
    (if (equal? name 'set-cdr!)
        (set-cdr! (car args) (cadr args))
    (if (equal? name 'memq)
        (memq (car args) (cadr args))
    (if (equal? name 'assq)
        (assq (car args) (cadr args))    
    (if (equal? name '=)
        (= (car args) (cadr args))
    (if (equal? name '+)
        (+ (car args) (cadr args))
    (if (equal? name '-)
        (- (car args) (cadr args))
    (if (equal? name '*)
        (* (car args) (cadr args))
    (if (equal? name 'null?)
        (null? (car args))
    (if (equal? name 'not)
        (not (car args))    
    (if (equal? name 'symbol?)
        (symbol? (car args))
    (if (equal? name 'list)
        args
    (if (equal? name (quote pair?))
        (pair? (car args))
    (if (equal? name 'read)
        (if (null? args) (read) (read (car args)))
    (if (equal? name 'eof-object?)
        (eof-object? (car args))
    (if (equal? name 'close-input-port)
        (close-input-port (car args))
    (if (equal? name 'newline)
        (newline)
    (if (equal? name 'write)
        (write (car args))
    (if (equal? name 'display)
        (display (car args))
    (if (equal? name 'begin)
        (begin (car args))
     (if (equal? name 'load)
        (load (car args))   
         "erreur apply primitive"))))))))))))))))))))))))))

(define mapper
  (lambda (f l)
    (if (null? l)
        '()
        (cons (f (car l)) (mapper f (cdr l))))))

(define primitive-identifiers
  (lambda ()
    '(car cdr + - * = set-car! set-cdr! memq assq null? equal? newline write display read symbol? list pair? not
      eof-object? close-input-port open-input-file load begin)))

(define make-primitive
  (lambda (op)
    (list 'primitive op)))

(define id (lambda (x) x))

(define initialize-global-env
  (lambda ()
    (set! global-env 
      (extend '()
          (primitive-identifiers)
          (mapper make-primitive
                  (primitive-identifiers)))) '() ))

; env recursif

(define openloop
  (lambda (read-prompt write-prompt)
    (begin 
    (display read-prompt)
    (evaluate (read) global-env
              (lambda (v)
                (begin
                (display write-prompt)
                (display v)
                (newline)
                (openloop read-prompt write-prompt)))))))

(define babel
  (lambda ()
       (begin
       (initialize-global-env)
       (set-cdr! global-env (cons (cons 'global-env global-env)
                           (cdr global-env)))
      (openloop "i0 " "i0 "))))
