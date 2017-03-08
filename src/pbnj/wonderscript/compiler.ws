; vim: ft=clojure
(module pbnj.wonderscript)

(define-function pbnj.wonderscript/compile [exp]
  (pbnj.jess/compile (ws->jess exp)))

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
                        [(ws->jess (xs 0)) (ws->jess (xs 1))]) )))

(define-function vector->jess [exp]
  (cons 'pbnj.core.vector (map exp ws->jess)))

(define-function set->jess [exp]
  (cons 'pbnj.core.set (map exp ws->jess)))

(define variable? symbol?)

(define-function variable->jess [sym]
  (let [ns (namespace sym)
        nm (name sym)]
    (if ns
      sym
      (symbol (str (current-module-name)) nm))))

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
        (list 'set! name (ws->jess value))
        (list 'var name (ws->jess value))))))

(define cond? (tag-predicate 'cond))

(define-function cond->jess
  ([pred conse] (list 'if (ws->jess pred) (ws->jess conse)))
  ([pred conse alt] (list 'if (ws->jess pred) (ws->jess conse) (ws->jess alt)))
  ([&body]
   (if (not= (mod (count body) 2) 0) (throw "Expecting an even number of arguments"))
   (let [pairs (reverse (pair body))
         alt? (lambda [xs] (= (first xs) :else))
         alt (first (filter pairs alt?))
         preds (cons alt (reject pairs alt?))]
     (reduce
       preds
       (lambda [l xs]
               (if (alt? l)
                 (cond->jess (xs 0) (xs 1) (ws->jess (second l)))
                 (first (cons (cond->jess (xs 0) (xs 1) l)))))))))

(define lambda? (tag-predicate 'lambda))

(define-function lambda-body [body]
  (reduce
    (reverse body)
    (lambda [l exp]
            (if (empty? l)
              (cons (list 'return (ws->jess exp)) l)
              (cons (ws->jess exp) l)))
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
  (list 'set! var (ws->jess val)))

(define variable-introspection? (tag-predicate 'defined?))

(define-function variable-introspection->jess [var]
  (list '!== (list 'type (ws->jess var)) "undefined"))

(define thrown-exception? (tag-predicate 'throw))

(define-function thrown-exception->jess [value]
  (list 'throw (ws->jess value)))

(define application? list?)

(define-function application->jess [fn &args]
  (cons (ws->jess fn) (map args ws->jess)))

(define-function ws->jess [exp_]
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
              (throw (str "ws->jess: invalid form: '" exp "'"))))))

(define pbnj.wonderscript/wsToJess ws->jess)
