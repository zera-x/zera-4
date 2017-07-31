; vim: ft=clojure
(ns examples.recur)

(define-function fib
  {:memoize true}
  [n]
  (cond (= n 0) 1
        (= n 1) 1
        :else
          (+ (fib (- n 1)) (fib (- n 2)))))

;(println (fib 15))

;(define factorial
;  (lambda
;    [n]
;    (loop [cnt n acc 1]
;      (if (= cnt 0)
;          acc
;        (again (sub1 cnt) (* acc cnt))))))

(define-function fact!
  ;{:memoize true}
  ([n] (fact! n 1))
  ([n acc]
   (if (= 0 n)
     acc
     (again (sub1 n) (* acc n)))))

;(println (fact! 10))

(define-function map*
  ;{:memoize str}
  [f col]
  (cond (empty? col) col
        :else
          (let [x (first col) xs (rest col) x* (f x)]
            (cons (f (first col)) (map* f (rest col))))))

(define-function times2 [x] (* x 2))

;(p (map* times2 (range 0 20)))


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

(let [fibs (map fib (range 50))
      pairs (partition 2 fibs)
      ratios (reduce (lambda [memo xs] (concat memo (apply / xs))) pairs [])]
  (do-each [ratio ratios]
    (println (str (join "/" (reverse ratio)) " = " (apply / (reverse ratio))))))
