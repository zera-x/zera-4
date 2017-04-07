; vim: ft=clojure
(require "../jess.ws")
(module pbnj.wonderscript)

(define *js-root-object* (if (= *platform* "nodejs") 'global 'window))

(define property-accessor? isPropertyAccessor)
(define property-assignment? isPropertyAssignment)
(define method-application? isMethodApplication)
(define class-instantiation? isClassInstantiation)

(define-function repl-pprint [exp]
  (pprint exp)
  (pbnj.wonderscript.repl/pprint exp))

(define-function property-accessor->jess
  [env obj prop]
  (list '.- (ws->jess obj env) (ws->jess prop env)))

(define-function property-assignment->jess
  [env obj prop value]
  (list '.-set! (ws->jess obj env) (ws->jess prop env) (ws->jess value env)))

(define-function method-application->jess
  [env obj method]
  (if (list? method)
    (cons '. (cons (ws->jess obj env) (cons (first method) (map (rest method) (lambda [x] (ws->jess x env))))))
    (list '. (ws->jess obj env) method)))

(define-function class-instantiation->jess
  [env klass &args]
  (cons 'new (cons (ws->jess klass env) (map args (lambda [x] (ws->jess x env))))))

(define-function compile [exp]
  (pbnj.jess/compile (ws->jess exp (pbnj/env))))

(define-function self-evaluating? [exp]
  (or (nil? exp) (number? exp) (boolean? exp) (string? exp) (date? exp) (regexp? exp)))

(define-function keyword->jess [exp]
  (list 'pbnj.core.keyword (namespace exp) (name exp)))

(define-function map->jess [exp env]
  (cons 'pbnj.core.hashMap
        (mapcat exp
                (lambda [xs]
                        [(ws->jess (xs 0) env) (ws->jess (xs 1) env)]) )))

(define-function vector->jess [exp env]
  (cons 'pbnj.core.vector (map exp (lambda [x] (ws->jess x env)))))

(define-function set->jess [exp env]
  (cons 'pbnj.core.set (map exp (lambda [x] (ws->jess x env)))))

(define variable? symbol?)

(define-function variable-ns [sym env]
  (let [scope (pbnj.wonderscript/lookupVariable sym env)]
    (if (or (nil? scope) (isEnv scope))
      nil
      (.- scope "@@NAME@@"))))

(define-function variable->jess [sym env]
  (let [ns (namespace sym)
        nm (name sym)]
    (cond ns
        (if (= ns "js")
          (symbol (str *js-root-object*) nm)
          sym)
      :else
        (let [ns (variable-ns sym env)
              ns-parts (if ns (. (str ns) (split ".")) (array))]
          (if ns
            (let [ns-parts (. (str ns) (split "."))]
              (list '.- *js-root-object* (into (vector) (concat ns-parts [nm]))))
            sym)))))

(define-function tag-predicate [tag]
  (lambda [exp] (= tag (first exp))))

(define quoted? (tag-predicate 'quote))

(define-function quoted->jess [value]
  (cons 'quote value))

(define definition? (tag-predicate 'define))

; TODO: add module resolution
(define-function definition->jess
  ([name env] (list 'var name))
  ([name value env]
    (let [ns (namespace name)]
      (if ns
        (list 'set! name (ws->jess value env))
        (list 'var name (ws->jess value env))))))

(define cond? (tag-predicate 'cond))

(define-function cond->jess
  ([env pred conse] (list 'if (ws->jess pred env) (ws->jess conse env)))
  ([env pred conse alt] (list 'if (ws->jess pred env) (ws->jess conse env) (ws->jess alt env)))
  ([env &body]
   (if (not= (mod (count body) 2) 0) (throw "Expecting an even number of arguments"))
   (let [pairs (reverse (pair body))
         alt? (lambda [xs] (= (first xs) :else))
         alt (first (filter pairs alt?))
         preds (cons alt (reject pairs alt?))]
     (reduce
       preds
       (lambda [l xs]
               (if (alt? l)
                 (cond->jess env (xs 0) (xs 1) (ws->jess (second l) env))
                 (first (cons (cond->jess env (xs 0) (xs 1) l)))))))))

(define lambda? (tag-predicate 'lambda))

; TODO: add capture arguments
; TODO: add recursion points, define-type, and define-protocol
(define-function lambda-body [body env]
  (reduce
    (reverse body)
    (lambda [l exp]
            (if (empty? l)
              (cons (list 'return (ws->jess exp env)) l)
              (cons (ws->jess exp env) l)))
    (list)))

(define-function lambda-single-body [args body env]
  (list 'paren
    (cons 'function
          (cons args
                (cons (list 'if-else
                            (list '!== 'arguments.length (count args))
                            (list 'throw "Wrong number of arguments"))
                (lambda-body body env))))))

(define-function lambda-multi-body [bodies env]
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
                                            (cons 'do (lambda-body (rest body) env))]))
                           [:else (list 'throw (list 'new 'Error "Wrong number of arguments"))])))))))

(define-function lambda->jess
  [args body env]
  (cond (vector? args) (lambda-single-body args body env)
        (list? args) (lambda-multi-body (cons args body) env)
        :else (throw "first argument should be a list or a vector")))

(define block? (tag-predicate 'do))

(define-function block->jess [env &exprs]
  (list 'paren (list (cons 'function (cons [] (lambda-body exprs env))))))

(define assignment? (tag-predicate 'set!))

(define-function assignment->jess [env var val]
  (list 'set! var (ws->jess val env)))

(define variable-introspection? (tag-predicate 'defined?))

(define-function variable-introspection->jess [env var]
  (list '!== (list 'type (ws->jess var env)) "undefined"))

(define thrown-exception? (tag-predicate 'throw))

(define-function thrown-exception->jess [env value]
  (list 'throw (ws->jess value env)))

(define application? list?)

(define-function application->jess [env fn &args]
  (let [fn_ (if (has? '#{+ - / * < > <= >= not mod} fn) fn (ws->jess fn env))]
    (cons fn_ (map args (lambda [x] (ws->jess x env))))))

(define-function ws->jess [exp_ env]
  (let [exp (macroexpand exp_)]
    (cond (self-evaluating? exp) exp
          (keyword? exp) (keyword->jess exp)
          (map? exp) (map->jess exp env)
          (vector? exp) (vector->jess exp env)
          (set? exp) (set->jess exp env)
          (variable? exp) (variable->jess exp env)
          (quoted? exp) (apply quoted->jess (rest exp))
          (definition? exp) (apply definition->jess (concat (rest exp) [env]))
          (cond? exp) (apply cond->jess (cons env (rest exp)))
          (lambda? exp) (lambda->jess (first (rest exp)) (rest (rest exp)) env)
          (block? exp) (apply block->jess (cons env (rest exp)))
          (assignment? exp) (apply assignment->jess (cons env (rest exp)))
          (variable-introspection? exp) (apply variable-introspection->jess (cons env (rest exp)))
          (thrown-exception? exp) (apply thrown-exception->jess (cons env (rest exp)))
          (property-accessor? exp) (apply property-accessor->jess (cons env (rest exp)))
          (property-assignment? exp) (apply property-assignment->jess (cons env (rest exp)))
          (method-application? exp) (apply method-application->jess (cons env (rest exp)))
          (class-instantiation? exp) (apply class-instantiation->jess (cons env (rest exp)))
          (application? exp) (apply application->jess (cons env exp))
          :else 
            (do
              (println exp)
              (throw (str "ws->jess: invalid form: '" exp "'"))))))
