; vim: ft=clojure
(module pbnj.node)

(define- *bluebird-api* (if (= *platform* :nodejs) (js.node/require "bluebird") js/Promise))

(define- *fs* (.promisifyAll *bluebird-api* (js.node/require "node")))

(define-function ->promise
  [value]
  (.resolve *bluebird-api* value))

(define-function read-file
  [file]
  (.readFileAsync *fs* file))
