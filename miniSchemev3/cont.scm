(definerec cfact
  (lambda (n)
    (lambda (k)
     (if (= n 0) (k 1)
         ((cfact (- n 1)) (lambda (v) (k (* n v))))))))

((cfact 5) (lambda (x) x))