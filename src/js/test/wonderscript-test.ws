; vim: ft=clojure
(module 'pbnj.wonderscript.test-suite)

(read-file "src/pbnj/wonderscript/test.ws")

;; Reference test suite for WonderScript implementations

(define-test self-evaluating
  (is (= 1 (read-string "2")))
  (is (= 3.14159 (read-string "3.14159")))
  (is (= 3000 (read-string "3_000")))
  (is (= "abc" (read-string "\"abc\"")))
  (is (= :a (read-string ":a")))
  (is (= 'b (read-string "'b")))
  (is (= '(1 2 3) (read-string "'(1 2 3)")))
  (is (= [1 2 3] (read-string "[1 2 3]")))
  (is (= {:a 1 :b 2 :c 3} (read-string "{:a 1 :b 2 :c 3}"))))

(define-test numbers
  (is (= 5000 (read-string "5,000")))
  (is (= 5000 (read-string "5_000")))
  (is (= 5 (read-string "5.000"))))
