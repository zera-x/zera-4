;; vim: ft=clojure
;; An http server using Node's API
(module examples.http)

(define http (js.node/require "http"))
(define host "127.0.0.1")
(define port 8000)

(define-function hello-server [req res]
  (.-set! res statusCode "200")
  (. res (setHeader "Content-type" "text/plain"))
  (. res (end "Hello, World\n")))

(define-function boot []
  (println (str "Server running at http://" host ":" port)))

(define server (. http (createServer hello-server)))
(. server (listen port host boot))
