(require "../src/pbnj/peanutbutter.ws")
(ns examples.pb)

(alias 'pb 'pbnj.peanutbutter)

;(pb/define-component greet [name] [:p "Hello, " name])

;(p (pb/component? '(greet "Test")))
;(p (pb/get-component 'greet))

;(say (pb/compile '(greet "Delon")))
(p (pb/html '(p {:class (muted well)} "Hello, Delon")))
;(p (pb/component? '[greet "Test"]))
;(say (pb/compile [:p "Hello, Delon"]))
