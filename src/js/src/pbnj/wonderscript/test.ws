; vim: ft=clojure
(module 'pbnj.wonderscript.test)

(define-syntax define-test [exp]
  (let [name (second exp)
        body (rest (rest exp))]
    (list 'define name (cons 'function (cons [] body))))) 

(define-syntax is [exp]
  (let [body (second exp)
        com (second (rest exp))]
    ;; TODO: improve this
    (list 'if-not body (list 'throw (str "FAILURE: " (inspect body) " is false")))))
