;; vim: ft=clojure
(module pbnj.wonderscript.repl-server)

(define- http (js.node/require "http"))
(define- host "127.0.0.1")
(define- port 8000)

(define-function- repl-server [req res]
  (.-set! res statusCode "200")
  (. res (setHeader "Content-type" "text/plain"))
  (. res (end "Hello, World\n")))

(define-function- boot []
  (println (str "Server running at http://" host ":" port)))

(define server (. http (createServer hello-server)))

(define-function run []
  (. server (listen port host boot)))
