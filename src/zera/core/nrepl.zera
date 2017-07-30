;; vim: ft=clojure
(ns pbnj.wonderscript.repl-server)

(alias 'ws 'pbnj.wonderscript)

(define-function raise [msg] (& (throw msg)))

(define server
  (let [net (js.node/require "net")]
    (lambda [f err &opts]
      (let [port (fn-opts opts :port (generate-int 1000 9999))
            host (fn-opts opts :host "127.0.0.1")
            s (.createServer net f)]
        (.on s "error" err)
        (.listen s
          port
          host
          (& (println "opened server on ", (.address s))))))))

(define-function greeting
  []
  (str "WonderScript " *version* "\n"
       *platform-version* "\n"
       "Running in " *mode* " mode\n"
       "   Exit: Control+D or exit\n"
       "   Help: (doc function-name-here)\n"
       "         (source function-name here)"))

(define-function read-service
  [s]
  (.write s (str (greeting) "\r\n"))
  (ns user)
  (.write s (str (ns-name *namespace*) "> "))
  (.on s "data"
    (lambda [data]
      (try
        (let [sdata (.trim (.toString data "utf8"))
              res (pbnj.wonderscript/readString sdata)]
          (say sdata " => " (inspect res))
          (.write s (str "=> " (inspect res) "\r\n")))
          (.write s (str (ns-name *namespace*) "> "))
        (catch [e js/Error]
          (.error js/console e)
          (.write s (.getMessage e)))))))

(server
  read-service ;(& (.end % "goodbye\n"))
  (lambda [error] (p error) (throw error))
  :port 4000)
