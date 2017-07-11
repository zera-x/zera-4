(module examples.jess)
(require "../src/pbnj/compiler.ws")

(define- compile pbnj.compiler/compile)
(define- lambda->jess pbnj.compiler/lambda->jess)

(p (compile {:a 1 :b 2}))
(p (compile {:a "1" :b "2"}))
(p (compile {:a "1" :b {:c 2 :d 3}}))
(p (compile {:a [1 2 3] :b {:c 2 :d 3}}))
(p (compile '(alert "Hello")))
(p (pbnj.compiler/lambda-single-body 'lambda '[x] 'x))
;(p (compile '(lambda [x] (lambda [] x))))
