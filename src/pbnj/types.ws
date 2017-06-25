; vim: ft=clojure
(module pbnj.types)

(define-type Symbol
  [ns name])

(define-type Keyword
  [ns name])

(define-protocol Seqable
  (seq [this]))

(define-protocol IPersistentCollection
  ; Seqable
  (count [this])
  (cons [this x])
  (empty? [this] (and (nil? (.first this)) (nil? (.rest this))))
  (equiv? [this x]))

(define-protocol IPersistentStack
  ; IPersistentCollection
  (peek [this])
  (pop [this]))

(define-protocol IReduce
  (reduce
    ([this fn] (.reduce this fn nil))
    ([this fn init]
     (if (.empty? this)
       init
       (let [l (atom (if (nil? init) (.rest this) this))
             init* (if (nil? init) (.first this) init)]
         (until (.empty? @l)
            (set! init* (fn init* (.first @l)))
            (swap! l (lambda [val] (.rest val))))
         init*)))))

(define-protocol Counted
  (count [this]))

(define-protocol Sequential) ; ??? (see https://github.com/clojure/clojure/blob/clojure-1.9.0-alpha14/src/jvm/clojure/lang/Sequential.java)

(define-protocol IPersistentList
  ; Sequential
  ; IPersistentStack
  )

(define-type PersistentList
  [head tail count]
  IPersistentList
  IReduce
  (count [this] (.- this count))
  (first [this] (.- this head))
  (rest [this] (.- this tail))
  (cons [this x] (PersistentList. x this (+ 1 count))))

(define EMPTY-LIST (PersistentList. nil nil 0))

(define list
  ([a] (PersistentList. a nil 1))
  ([a b] (PersistentList. a b 2))
  ([a b c] (.cons (list b c) a)))

(define-type PersistentLazyList
  [head tail]
  IList
  (first [this] ((.- this head)))
  (rest [this] ((.- this tail)))
  (cons [this x] (PersistentLazyList. x (always this)))
  (map [this fn]))

(define EMPTY-LAZY-LIST (PersistentLazyList. (always nil) (always nil)))

(define-function lazy-list
  [head tail]
  (PersistentLazyList. head tail))

(define-function first [col]
  (.? col first))

(define-function second
  [col]
  (.first (.rest col)))

(define-function third
  [col]
  (.first (.rest (.rest col))))

(define-function fourth
  [col]
  (.first (.rest (.rest (.rest col)))))

(define-function rest [col]
  (.? col rest))

(define-function cons
  [x col]
  (if (nil? col)
    (PersistentList. x nil 1)
    (.? col (cons x)
      (throw (new js/Error (str "'" (inspect col) "' is not cons-able"))))))

(define-function conj
  [col x]
  (if (nil? col)
    (PersistentList. x nil 1)
    (.? col (cons x)
      (throw (new js/Error (str "'" (inspect col) "' is not conj-able"))))))

(define-function empty?
  [col]
  (.? col empty?
      (throw (new js/Error (str "'" (inspect col) "' empty? method not defined")))))

;(define-function reverse [col]
;  (let [xs col, ys PersistentList/EMPTY]
;    (while (= (count xs) 0)
;      (set! ys (. ys (cons (first xs))))
;      (set! xs (rest xs)))
;    ys))

(define-function mori-list->list
  [l]
  (let [xs (pbnj.core/reverse l), ys EMPTY-LIST]
    (println (count xs))
    (while (= (count xs) 0)
      (set! ys (. ys (cons (pbnj.core/first xs))))
      (set! xs (pbnj.core/rest xs)))
    ys))

(define-function reduce
  ([fn col]
   (.reduce col fn nil))
  ([fn col init]
   (.reduce col fn init)))
