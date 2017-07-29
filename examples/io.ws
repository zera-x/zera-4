(require "../src/pbnj/core/system.ws")
(module examples.io)
(use pbnj.core.system)

(unless (= (count *argv*) 2)
  (say "USAGE: examples/io.ws FILE")
  (process/exit 1))

(>> (slurp (*argv* 1))
    JSON/parse
    println)
