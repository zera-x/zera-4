; vim: ft=clojure
(module pbnj.core.io)

; syntax for "then" chains
; (!!> (.get client "usrs/self")
;      println)
; =>
; (.. (.get client "usrs/self")
;     (then println))
(define-macro !!>
  [ref &forms])
