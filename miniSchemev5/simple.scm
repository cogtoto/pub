(define void
 (lambda () (cons '* '*)))

(define caar (lambda (l) (car (car l))))
(define cadr (lambda (l) (car (cdr l))))
(define caddr (lambda (l) (car (cdr (cdr l)))))
(define cadddr (lambda (l) (car (cdr (cdr (cdr l))))))
(define cddr (lambda (l) (cdr (cdr l))))
(define cdddr (lambda (l) (cdr (cdr (cdr l)))))
(define cdar (lambda (l) (cdr (car l))))
(define cadar (lambda (l) (car (cdr (car l)))))

(define assq (lambda (e l)
 (if (null? l) 
      #f
     (if (equal? e (car (car l)))
      (car l)
      (assq e (cdr l))))))
 
(define global-env '())

(define evaluate
  (lambda (e r k)
    ((if (not (pair? e))
          (if (or (or (number? e) (string? e)) (boolean? e))
              eval-constante
              eval-variable)
          (if (equal? (car e) 'quote)
               eval-quote    
          (if (equal? (car e) 'if)
               eval-if
                (if (equal? (car e) 'begin)
               eval-begin
               (if (equal? (car e) 'define)
                   eval-assign
                   (if (equal? (car e) 'lambda)
                       eval-abstraction
                       eval-application))))))
      e r k )))

(define eval-constante
  (lambda (e r k)
    (k e)))

(define eval-quote
  (lambda (e r k)
    (k (cadr e))))

(define eval-variable
  (lambda (e r k)
   (get-pair e r
   (lambda (success-pair)
    (k (cdr success-pair)))
    (lambda ()
    (wrong "symbol not bound " e)))))

(define eval-if
  (lambda (e r k)
    (evaluate (cadr e) r
              (lambda (v)
                (if v
                    (evaluate (caddr e) r k)
                    (evaluate (cadddr e) r k))))))
                    
(define wrong
  (lambda (message object)
    (begin
     (display "erreur")
     (display message)
     (display object)
     (newline))))

(define get-pair
  (lambda (id r success failure)
    (find-pair id r
               success
               (lambda ()
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
                               (begin
                               (set-cdr! success-pair v)
                               (k (void)) ))
                             (lambda ()
                               (begin
                              (set-cdr! global-env (cons (car global-env)(cdr global-env)))
                              (set-car! global-env (cons (cadr e) v))
                              (k (void)))))))))
               
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
                (if (equal? (car proc) 'reifier)
                    ((reifier-to-cloture proc) (cdr e) r k)
                    (evlis (cdr e) r
                       (lambda (args)
                         (apply-procedure proc args k))))))))

(define apply-procedure
  (lambda (proc args k)
    (if (equal? (car proc) 'cloture)
        (eprogn (list (caddr proc))
                (extend (cadddr proc) (cadr proc) args)
                k)
        (k (apply-primitive (cadr proc) args))))) 

(define evlis
  (lambda (e r k)
    (if (null? e)
        (k '())
        (evaluate (car e) r
                  (lambda (v)
                    (evlis (cdr e) r
                           (lambda (w)
                            (k (cons v w)))))))))

(define eval-begin
  (lambda (e r k)
    (eprogn (cdr e) r k)))

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
         
(define apply-primitive
  (lambda (name args)
    (if (equal? name 'car)
        (car (car args))
     (if (equal? name 'or)
        (or (car args) (cadr args))
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
    (if (equal? name 'pair?)
        (pair? (car args))
    (if (equal? name 'read)
        (if (null? args) (read) (read (car args)))
    (if (equal? name 'newline)
        (newline)
    (if (equal? name 'equal?)
        (equal? (car args) (cadr args)) 
    (if (equal? name 'write)
        (write (car args))
    (if (equal? name 'display)
        (display (car args))
         (if (equal? name 'load)
        (load (car args)) 
       (if (equal? name 'number?)
        (number? (car args))   
          (if (equal? name 'string?)
        (string? (car args))   
          (if (equal? name 'boolean?)
        (boolean? (car args))   

         "erreur apply primitive"))))))))))))))))))))))))))))

(define mapper
  (lambda (f l)
    (if (null? l)
        '()
        (cons (f (car l)) (mapper f (cdr l))))))

(define primitive-identifiers
  (lambda ()
    '(placeholder car cdr cons + - * = set-car! set-cdr! memq assq null? equal? newline write display read 
     symbol? list pair? not load  or number? string? boolean?)))

(define make-primitive
  (lambda (op)
    (list 'primitive op)))

(define reifier-to-cloture
  (lambda (reifier)
    (cons 'cloture (cdr reifier))))

(define cloture-to-reifier
  (lambda (cloture)
    (cons 'reifier (cdr cloture))))

(define make-reifier
  (lambda (formals body r)
    (list 'reifier formals body r)))

(define id (lambda (x) x))

(define global-env '())

(define initialize-global-env
  (lambda ()
     (define global-env
      (extend global-env 
          (primitive-identifiers)
          (mapper make-primitive
                  (primitive-identifiers)))))) 

(define openloop
  (lambda (read-prompt write-prompt)
    (begin 
    (display read-prompt)
    (evaluate (read) '()
              (lambda (v)
                (begin
                (display write-prompt)
                (if (equal? v (void))
                  "rien a afficher"
                  (display v))
                (newline)
                (openloop read-prompt write-prompt)))))))

(define global-env (cons (cons 'placeholder  999) '()))

(initialize-global-env)

(define babel
  (lambda ()
       (begin
       (set-car! global-env (cons 'global-env global-env ))
       (openloop "i0 " "i0 "))))
