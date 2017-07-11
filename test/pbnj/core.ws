; vim: ft=clojure
(module pbnj.core)

(define-test cond
  (is (= (cond) nil))
  (try
    (is (cond 1) nil)
    (is false "Should have throw an exception")
    (catch [e js/Error]
      (is true)))
  (is (= (cond 1 2) 2))
  (is (= (cond nil 2 3 4) 4))
  (is (= (cond nil 2 false 4 5 6) 6))
  (is (= (cond nil 2 false 4 true 6) 6))
  (is (= (cond nil 2 false 4 :else 6) 6)))

(define-test .?
  (let [d (new js/Date 2016 10 25)]
    (is (= (.? d getMonth) 10))
    (is (= (.? d missing-method) nil))
    (.? d (setMonth 11))
    (is (= (.? d (getMonth)) 11))))

(define-test ..
  (let [xs (array 1 2 3 4 5)]
    (.. "1,2,3,4,5"
        (split ",")
        (map (lambda [x &rest] (* 1 x)))
        (forEach (lambda [x i other] (is (= x (.- xs i))))))))

(define-test ..?
  (let [xs (array 1 2 3 4 5)]
    (..? "1,2,3,4,5"
         (split ",")
         (map (lambda [x &rest] (* 1 x)))
         (forEach (lambda [x i other]
                       (println "x" x "xs" xs (str "xs[" i "]") (.- xs i))
                       (is (= x (.- xs i)))))))
  (is (= nil (..? "1,2,3,4,5" (split ",") (missing-method 1 2 3 4 5)))))

(define-test read-string
  (let [n (generate-nat)]
    (is (= n (read-string (str n)))))
  (is (= -1 (read-string "-1")))
  (is (= -1.14 (read-string "-1.14")))
  (is (= 1.14 (read-string "+1.14")))
  (is (= 1 (read-string "+1")))
  (is (= 3.14159 (read-string "3.14159")))
  (is (= 3000 (read-string "3_000")))
  (is (= "abc" (read-string "\"abc\"")))
  (is (= :a (read-string ":a")))
  (is (= 'b (read-string "'b")))
  (is (= '(1 2 3) (read-string "'(1 2 3)")))
  (is (= [1 2 3] (read-string "[1 2 3]")))
  (is (= {:a 1 :b 2 :c 3} (read-string "{:a 1 :b 2 :c 3}")))
  (is (= 5000 (read-string "5,000")))
  (is (= 5000 (read-string "5_000")))
  (is (= 5 (read-string "5.000"))))

(define-test comment
  (is (= nil (comment)))
  (is (= nil (comment asdfasdf asfasdf sfasdfasd asfasdfasd))))

(define-test let
  (is (= [1 2]
         (let [x 1
               y (+ x 1)]
           (is (= x 1))
           (is (= y 2))
           [x y]))))

(define-test if
  (is (= 1 (if true 1)))
  (is (= 1 (if true 1 2)))
  (is (= 2 (if false 1 2)))
  (is (= nil (if false 1)))
  (is (= 2 (if nil 1 2))))

(define-macro if-not
  ([pred conse] (list 'cond (list 'not pred) conse))
  ([pred conse alt] (list 'cond (list 'not pred) conse :else alt)))

(define-test if-not
  (is (= 1 (if-not false 1)))
  (is (= 1 (if-not false 1 2)))
  (is (= 2 (if-not true 1 2)))
  (is (= nil (if-not true 1)))
  (is (= 1 (if-not nil 1 2))))

(define-macro unless [pred &acts]
  (list 'cond (list 'not pred) (cons 'do acts)))

(define-test unless
  (is (= 5 (unless false 1 2 3 4 5)))
  (is (= nil (unless true 1 2 3 4 5))))

(define-test when
  (is (= 5 (when true 1 2 3 4 5)))
  (is (= nil (when false 1 2 3 4 5))))

(define-test or
  (is (or true))
  (is (or false true))
  (is (or false false true))
  (is (or false false false true))
  (is-not (or false))
  (is-not (or false false))
  (is-not (or false false false)))

(define-test and
  (is (and true))
  (is (and true true))
  (is (and true true true))
  (is-not (and false))
  (is-not (and false false))
  (is-not (and false false false))
  (is-not (and false true))
  (is-not (and false true true))
  (is-not (and true true false)))

(define-test define-function
  (define-function ident [x] x)
  (define-function inc [x] (+ 1 x))
  (is (= 1 (ident 1)))
  (is (= :a (pbnj.core/ident :a)))
  (is (= 4 (inc 3)))
  (is (= 5 (pbnj.core/inc 4))))

(define-test define-function-
  (define-function- ident- [x] x)
  (define-function- inc- [x] (+ 1 x))
  ;(is (= :a (pbnj.core/ident- :a)))
  ;(is (= 5 (pbnj.core/+1- 4)))
  (is (= 1 (ident- 1)))
  (is (= 4 (inc- 3))))

(define-test not=
  (is (not= 1 2))
  (is (not= :a :b))
  (is (not= 1 :a))
  (is-not (not= 1 1))
  (is-not (not= :a :a))
  (is-not (not= [1 2 3 4] [1 2 3 4])))

(define-test gen-sym
  (is (symbol? (gen-sym)))
  (is (symbol? (gen-sym "prefix")))
  (is-not (= (gen-sym) (gen-sym)))
  (is-not (= (gen-sym "prefix") (gen-sym "prefix"))))

(define-test natural?
  (is (natural? 0))
  (is (natural? 1))
  (is (natural? 34))
  (is (natural? 21412412341234123463456435437456))
  (is-not (natural? nil))
  (is-not (natrual? "1"))
  (is-not (natrual? true))
  (is-not (natural? -1))
  (is-not (natrual? 1.1)))

(define-test generate-int
  (do-times [n 100]
    (is (integer? (generate-int)))))

(define-test generate-nat
  (do-times [n 20]
    (let [m (generate-nat)]
      (is (natural? (generate-nat))))))

(define-test generate-float
  (do-times [n 20]
    (is (number? (generate-float)))))

(define-test generate-str
  (do-times [n 20]
    (is (string? (generate-str)))))

(define-test generate-keyword
  (do-times [n 20]
    (is (keyword? (generate-keyword)))))

(define-test generate-symbol
  (do-times [n 20]
    (is (symbol? (generate-symbol)))))

(define-generic t
  "A generic function example"
  string?)
(define-method t false [_] :not-string)
(define-method t true [_] :string)

(define-protocol ITest0
  IMeta
  IRef
  IDeref)
(define-type Test0 [] ITest0)

(define-protocol ITest1
  IDeref
  (unwrap [self] (.-value self)))

(define-protocol ITest2
  "This is my third test of protocols"
  IDeref
  (unwrap [self] (.-value self)))

(define-protocol ITest3
  "This is my fourth test of protocols"
  {:added "4th"}
  IDeref
  (unwrap [self] (.-value self)))

(define-type Test1 [value] ITest1)
