(openloop "i1? " "i1> ")
(openloop "i2? " "i2> ")
(openloop "i3? " "i3> ")

(define exit (cloture-to-reifier (lambda (e r k) (car e))))

(define callcc (cloture-to-reifier (lambda (e r k) ((evaluate (car e) r id) k))))
(define myquote (cloture-to-reifier (lambda (e r k) (k (car e) ))))

(define maliste (cloture-to-reifier (lambda (e r k) (k (list (car e) k)))))

(define maliste (cloture-to-reifier (lambda (e r k) ( k ) )))

(define call/cc call-with-current-continuation)
(call/cc (lambda (k) (k 12)))

(define fact (lambda (n) (if (= n 0)  1 (* n (fact (- n 1))))))

(letrec ((facm (lambda (n) (if (= n 0)  1 (* n (facm (- n 1)))))))
   (facm 5))

(+ 5 (callcc (lambda (k) (k (* 2 8)))))

(+ 5 (callcc (lambda (k) (* 2 (k 8)))))

(+ 5 (callcc (lambda (k) (* 2 (k 8)))))

(* 4 (+ (callcc (lambda (k) (* (k 5) 8))) 2))

((cloture-to-reifier (lambda (e r k) (list e ))) (* 3 5)) 