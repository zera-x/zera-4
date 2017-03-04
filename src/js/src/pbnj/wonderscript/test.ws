; vim: ft=clojure
(module 'pbnj.wonderscript.test)

(define-macro define-test [name &body]
  (list 'define name (cons 'function (cons [] body))))

(define-macro is [body com]
  (list 'if-not body (list 'throw (str "FAILURE: " (inspect body) " is false"))))
