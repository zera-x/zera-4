(ns examples.fib)


(defn fib
  [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else
          (+ (again (- n 1)) (again (- n 2)))))

(defn map*
  [f col]
  (cond (empty? col) col
        :else
          (let [x (first col) xs (rest col) x* (f x)]
            (cons (f (first col)) (recur f (rest col))))))

(defn times2 [x] (* x 2))

;(.-set! pbnj.core DEBUG true)
;(println
;  (loop [n 10]
;    (cond (= n 0) 0
;          (= n 1) 1
;          :else
;            (let [n1 (again (- n 1))
;                  n2 (again (- n 2))]
;              (+ n1 n2)))))

(println (map* times2 (range 5)))
