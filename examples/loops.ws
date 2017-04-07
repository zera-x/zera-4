(module examples.loops)

(define exp '(do-times [i 10] (println "i:" i)))

(println "exp:" (inspect exp))
(println "expansion:" (inspect (macroexpand exp)))
;(.-set! pbnj.core DEBUG true)
(eval exp)
