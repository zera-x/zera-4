(ns pbnj.core.db)

(define-function- parse-url
  [url]
  (let [u (js.node/require "url")]
    (object->map (.parse u url))))

(define-function- load-sqlite-db
  [url]
  (let [sqlite3 (.verbose (js.node/require "sqlite3"))
        host (url :host)
        path (url :path)]
    (new (.- sqlite3 Database)
         (if (= "memory" host) ":memory:"
           (if path (.replace (str host path) "%20" " ") host)))))

(define- *schemes*
  {"sqlite:" load-sqlite-db})

(define-function connect
  [url]
  (let [url* (parse-url url)
        connector (*schemes* (url* :protocol))]
    (connector url*)))
