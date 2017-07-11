(module examples.jess)
(require "../src/pbnj/jess.ws")

(define- compile pbnj.jess/compile)

(p (compile {:a 1 :b 2}))
(p (compile {:a "1" :b "2"}))
(p (compile {:a "1" :b {:c 2 :d 3}}))
(p (compile {:a [1 2 3] :b {:c 2 :d 3}}))
(p (compile '(alert "Hello")))
(p (compile '(function ident [x] (return x))))
(p (compile '(function always [x] (return (var fn (function [] (return x)))))))
