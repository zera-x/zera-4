; vim: ft=clojure
(module pbnj.types)

(define-type PersistentList
  [h t]
  (first [self] (.- self h))
  (rest [self] (.- self t))
  (empty? [self] (and (nil? (.- self h)) (nil? (.- self t))))
  (conj [self x] (new PersistentList x self))
  (cons [self x] (new PersistentList x self))
  (reduce
    ([self fn] (. self (reduce fn nil)))
    ([self fn init]
     (if (. self empty?)
       init
       (let [s self, init* init]
         (when (nil? init*)
           (set! init* (. s first))
           (set! s (. s rest)))
         (until (. s empty?)
           (println s init*)
           (set! init* (fn init* (. s first)))
           (set! s (. s rest)))
         init*)))))

(define *EmptyList* (new PersistentList nil nil))

(define-function first [col]
  (.? col first
      (throw (new js/Error (str "'" (inspect col) "' first method not defined")))))

(define-function rest [col]
  (.? col rest
      (throw (new js/Error (str "'" (inspect col) "' rest method not defined")))))

(define-function cons
  [x col]
  (.? col (cons x)
      (throw (new js/Error (str "'" (inspect col) "' is not cons-able")))))

(define-function conj
  [col x]
  (.? col (conj x)
      (throw (new js/Error (str "'" (inspect col) "' is not conj-able")))))

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

(define-function list
  [&args]
  (let [xs (pbnj.core/reverse args), ys *EmptyList*]
    (println (count xs))
    (while (= (count xs) 0)
      (set! ys (. ys (cons (pbnj.core/first xs))))
      (set! xs (pbnj.core/rest xs)))
    ys))

(define-function reduce
  ([fn col]
   (. col (reduce fn)))
  ([fn col init]
   (. col (reduce fn init))))
