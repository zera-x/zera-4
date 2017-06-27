; vim: ft=clojure
(require "../src/pbnj/types.ws")
(module examples.types)

(define xs (pbnj.types/list 1 2 3))

;(println (pbnj.types/reduce + xs))

(println (pbnj.types/first xs))
