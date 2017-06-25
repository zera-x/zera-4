; vim: ft=clojure
(module examples.recur)

(define fib
  (memoize
    (lambda
      [n]
      (cond (= n 0) 0
            (= n 1) 1
            :else
              (+ (fib (- n 1)) (fib (- n 2)))))))

(define factorial
  (lambda
    [n]
    (loop [cnt n acc 1]
      (if (= cnt 0)
          acc
        (again (sub1 cnt) (* acc cnt))))))

(define-function fact!
  ([n] (fact! n 1))
  ([n acc]
   (if (= 0 n)
     acc
     (again (sub1 n) (* acc n)))))

(define-function map*
  [f col]
  (cond (empty? col) col
        :else
          (let [x (first col) xs (rest col) x* (f x)]
            (cons (f (first col)) (map* f (rest col))))))

(define-function times2 [x] (* x 2))

(println (fact! 5))

;(.-set! pbnj.core DEBUG true)
;(println
;  (loop [n 10]
;    (cond (= n 0) 0
;          (= n 1) 1
;          :else
;            (let [n1 (again (- n 1))
;                  n2 (again (- n 2))]
;              (+ n1 n2)))))

;(println (map* times2 (range 5)))

;(println (fib 5))

;(let [fibs (map (range 20) fib)
;      pairs (pair fibs)
;      ratios (reduce pairs (lambda [memo xs] (concat memo (apply / (reverse xs)))) [])]
;  (do-each [ratio ratios]
;    (println (str ratio))))
