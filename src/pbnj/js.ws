(ns js)

(define-function object
  [&pairs]
  (let [o (.create js/Object js/null)
        xs (partition 2 pairs)]
    (do-each [x xs]
      (let [k (first x)]
        (.-set! o (if (keyword? k) (name k) (str k)) (second x))))
    o))

; TODO: build with js/function and js/object
(define-function create-type [attrs &specs])
(define-function create-protocol [attrs &specs])
