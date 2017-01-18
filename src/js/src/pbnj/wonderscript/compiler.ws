; vim: set ft=clojure
(module 'pbnj.wonderscript.compiler)

(define-function pbnj.wonderscript.compiler/compile [exp]
  (emit-js (macroexpand exp)))

(define-function emit-js [exp]
  )
