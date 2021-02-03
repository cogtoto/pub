(define cf
  (lambda (n k)
     (if (= n 0) (k 1)
          (cf (- n 1) (lambda (v) (k (* n v)))))))

(cf 5 (lambda (x) x))