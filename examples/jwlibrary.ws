; vim: ft=clojure
(require "../src/pbnj/honey.ws")
(module jwlibrary)
(use pbnj.types)

;(define- *db* (connect "sqlite://examples/UserDataBackup_2017-06-23_cherub2.jwlibrary"))

;(println *db*)
(define xs (PersistentList. 1 (PersistentList. 2 EMPTY-LIST)))
(println (.reduce xs + 0))
