(require "../src/pbnj/core/system.ws")
(require "../src/pbnj/jess/estree.ws")
(require "../src/pbnj/jess.ws")
(module examples.jsparse)

(unless (= (count *argv*) 2)
  (say "USAGE: examples/io.ws FILE")
  (process/exit 1))

(define es (js.node/require "acorn"))
(define jscode (pbnj.core.system/slurp (*argv* 1) :sync))
(define tree (.parse es jscode))
;(define tree (.parse es "console.log('hello')"))

(p (pbnj.jess.estree/estree->jess tree))
