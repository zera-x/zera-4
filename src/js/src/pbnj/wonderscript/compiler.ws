; vim: ft=clojure
(module 'pbnj.wonderscript)

(define-function pbnj.wonderscript/compile [exp]
  (pbnj.jess/compile (emit-jess exp)))

(define-function self-evaluating? [exp]
  (or (nil? exp) (number? exp) (boolean? exp) (string? exp) (date? exp) (regexp? exp)))

(define-function symbol->jess [exp]
  exp)
  ;(list 'pbnj.core.symbol (namespace exp) (name exp)))

(define-function keyword->jess [exp]
  (list 'pbnj.core.keyword (namespace exp) (name exp)))

(define-function map->jess [exp]
  (cons 'pbnj.core.hashMap
        (mapcat exp
                (lambda [xs]
                        [(emit-jess (xs 0)) (emit-jess (xs 1))]) )))

(define-function vector->jess [exp]
  (cons 'pbnj.core.vector (map exp emit-jess)))

(define-function set->jess [exp]
  (cons 'pbnj.core.set (map exp emit-jess)))

(define variable? symbol?)

(define variable->jess symbol->jess)

(define-function tag-predicate [tag]
  (lambda [exp] (= tag (first exp))))

(define quoted? (tag-predicate 'quote))

(define-function quoted->jess [value]
  (cons 'quote value))

(define definition? (tag-predicate 'define))

; TODO: add module resolution
(define-function definition->jess
  ([name] (list 'var name))
  ([name value]
    (let [ns (namespace name)]
      (if ns
        (list 'set! name (emit-jess value))
        (list 'var name (emit-jess value))))))

(define cond? (tag-predicate 'cond))

(define-function cond->jess [&body]
  (list 'paren
    (list
      (list 'function []
            (cons 'if-else
                  (mapcat
                    (pair body)
                    (lambda [xs]
                            [(if (= :else (xs 0)) :else (emit-jess (xs 0)))
                             (list 'return (emit-jess (xs 1)))])))))))

(define lambda? (tag-predicate 'lambda))

(define-function lambda-body [body]
  (reduce (reverse body)
          (lambda [l exp]
                  (if (empty? l) (cons (list 'return (emit-jess exp)) l) (cons (emit-jess exp) l)))
          (list)))

(define-function lambda-single-body [args body]
  (list 'paren
    (cons 'function
          (cons args
                (cons (list 'if-else
                            (list '!== 'arguments.length (count args))
                            (list 'throw "Wrong number of arguments"))
                (lambda-body body))))))

(define-function lambda-multi-body [bodies]
  (let [arg-list (map (sort-by second (map bodies (lambda [b] [(first b) (count (first b))]))) first)
        longest (last arg-list)
        shortest (first arg-list)]
    (list 'paren
       (cons 'function
             (list longest
                   (cons 'if-else
                         (concat
                           (mapcat bodies
                                   (lambda [body]
                                           [(list '=== 'arguments.length (count (first body)))
                                            (cons 'do (lambda-body (rest body)))]))
                           [:else (list 'throw (list 'new 'Error "Wrong number of arguments"))])))))))

(define-function lambda->jess
  [args &body]
  (cond (vector? args) (lambda-single-body args body)
        (list? args) (lambda-multi-body (cons args body))
        :else (throw "first argument should be a list or a vector")))

(define block? (tag-predicate 'do))

(define-function block->jess [&exprs]
  (list 'paren (list (cons 'function (cons [] (lambda-body exprs))))))

(define assignment? (tag-predicate 'set!))

(define-function assignment->jess [var val]
  (list 'set! var (emit-jess val)))

(define variable-introspection? (tag-predicate 'defined?))

(define-function variable-introspection->jess [var]
  (list '!== (list 'type (emit-jess var)) "undefined"))

(define thrown-exception? (tag-predicate 'throw))

(define-function thrown-exception->jess [value]
  (list 'throw (emit-jess value)))

(define application? list?)

(define-function application->jess [fn &args]
  (cons (emit-jess fn) (map args emit-jess)))

(define-function pbnj.wonderscript/emit-jess [exp_]
  (let [exp (pbnj.wonderscript/macroexpand exp_)]
    (cond (self-evaluating? exp) exp
          (keyword? exp) (keyword->jess exp)
          (symbol? exp) (symbol->jess exp)
          (map? exp) (map->jess exp)
          (vector? exp) (vector->jess exp)
          (set? exp) (set->jess exp)
          (variable? exp) (variable->jess exp)
          (quoted? exp) (apply quoted->jess (rest exp))
          (definition? exp) (apply definition->jess (rest exp))
          (cond? exp) (apply cond->jess (rest exp))
          (lambda? exp) (apply lambda->jess (rest exp))
          (block? exp) (apply block->jess (rest exp))
          (assignment? exp) (apply assignment->jess (rest exp))
          (variable-introspection? exp) (apply variable-introspection->jess (rest exp))
          (thrown-exception? exp) (apply thrown-exception->jess (rest exp))
          (application? exp) (apply application->jess exp)
          :else 
            (do
              (println exp)
              (throw (str "emit-jess: invalid form: '" exp "'"))))))
