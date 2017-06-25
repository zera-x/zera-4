; vim: ft=clojure
(require "types.ws")
(module pbnj.honey)

(define-function- parse-url
  [url]
  (let [u (js.node/require "url")]
    (object->map (.parse u url))))

(define-function- load-sqlite-db
  [url]
  (let [sqlite3 (.verbose (js.node/require "sqlite3"))
        host (url :host)]
    (new (.- sqlite3 Database)
         (if (= "memory" host) ":memory" host))))

(define- *schemes*
  {"sqlite:" load-sqlite-db})

(define-function connect
  [url]
  (let [url* (parse-url url)
        connector (*schemes* (url* :protocol))]
    (connector url*)))

(define-function sql-query
  [db sql]
  (let [first 
  (.each
    db
    sql
    (lambda [err row]
            )))
