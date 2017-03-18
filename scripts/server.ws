;; vim: ft=clojure
(define- express (js.node/require "express"))
(define- app (express))

(. app (use "/" (. express (static (str js.node/__dirname "/../dist")))))
(. app (listen 3000 (lambda [] (println "listening"))))
