;; vim: ft=clojure
(require "../src/pbnj/peanutbutter.ws")
(require "../src/pbnj/http-service.ws")
(require "../src/pbnj/wonderscript/repl.ws")

;; FIXME: SCOPING BUG 
;; Without module declaration below *module-name* is still pbnj.core where as (current-module-name) is pbnj.user
;; causes defined variables to come up "undefined"
;(println *module-name*)
(module pbnj.wonderscript.repl-service)
(use pbnj.http-service)

(define- html pbnj.peanutbutter/html)

(define-service repl-service
  "A REPL web service for WonderScript"
  (GET "/?" [req]
       (html [:pbnj.wonderscript.repl/layout
              [:pbnj.wonderscript.repl/repl]]))
  (GET "/:name" [req]
       (str "Hello, " (get-in req [:params :name] "World"))))

(start repl-service 3000
       (lambda [] (println "WonderScript REPL Service - listening at http://localhost:3000")))
