(module examples.continuations)

(define-function sleep [ms]
  (suspend (lambda [resume]
                   (js/setTimeout resume ms))))

(sleep 1000)
;(.log js/console pbnj.wonderscript/RESTORE)

;(println (js/Date.))
;(println (js/Date.))
