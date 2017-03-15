; vim: ft=clojure
(module pbnj.types)

(define-class PersistentList
  [h t]
  (first [self] (.- self h))
  (rest [self] (.- self t))
  (empty? [self] (and (nil? (.- self h)) (nil? (.- self t))))
  (conj [self x] (new PersistentList x self))
  (cons [self x] (new PersistentList x self))
  (reduce
    ; TODO: fix multibody not working
    ([self fn] (. self reduce fn nil))
    ([self fn init])
     (if (. self empty?)
       init
       (let [s self
             init_ init]
         (do-while [(not (. s empty?))]
                   (set! init_ (fn init (. s first)))
                   (set! s (rest s)))
         init_))))

(define *EmptyList* (new PersistentList nil nil))

(println PersistentList)

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
