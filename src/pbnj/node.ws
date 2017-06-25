; vim: ft=clojure
(module pbnj.node)

(define- *bluebird-api*
  (if (= *platform* :nodejs) (js.node/require "bluebird") js/Promise))

(define- *fs* (.promisifyAll *bluebird-api* (js.node/require "fs")))

(define-function ->promise
  [value]
  (.resolve *bluebird-api* value))

(define-function ->io
  [value]
  (lambda [f] (.then value f)))

(define-function read-file
  [file]
  (-> (.readFileAsync *fs* file)
      (.then (lambda [x] (.toString x)))))

(define-function println
  [x]
  (if (.-then x)
    (.then x (lambda [data] (console/log data)))
    (console/log x)))
