; vim: ft=clojure
(require "syntax.ws")
(ns pbnj.wonderscript.analyzer)

(alias 'ws 'pbnj.wonderscript)
(alias 's 'pbnj.wonderscript.syntax)

(define property-accessor? ws/isPropertyAccessor)
(define property-assignment? ws/isPropertyAssignment)
(define method-application? ws/isMethodApplication)
(define class-instantiation? ws/isClassInstantiation)

(define variable? symbol?)

(define-function tag-predicate [tag]
  (lambda [exp] (= tag (first exp))))

(define quoted? (tag-predicate 'quote))

(define definition? (tag-predicate 'define))

(define if? (tag-predicate 'if))

(define lambda? (tag-predicate 'lambda))

(define block? (tag-predicate 'do))

(define assignment? (tag-predicate 'set!))

(define variable-introspection? (tag-predicate 'defined?))

(define thrown-exception? (tag-predicate 'throw))

(define application? list?)

(define-function analyze-application [fn &args]
  (s/Application. fn args))

(define-function analyze-nil
  [x]
  (s/Nil.))

(define-function analyze-number
  [x]
  (s/Number. x))

(define-function analyze-boolean
  [x]
  (s/Boolean. x))

(define-function analyze-string
  [x]
  (s/String. x))

(define-function analyze-date
  [x]
  (s/Instant. x))

(define-function analyze-regexp
  [x]
  (s/RegExp. x))

(define-function analyze-keyword
  [x]
  (s/Keyword. (namespace x) (name x)))

(define-function analyze-symbol
  [x]
  (s/Symbol. (namespace x) (name x)))

(define-function analyze-map-entries
  [x]
  [(analyze (x 0)) (analyze (x 1))])

(define-function analyze-map
  [x]
  (s/Map. (map analyze-map-entries x)))

(define-function analyze-vector
  [x]
  (s/Vector. (map analyze x)))

(define-function analyze-set
  [x]
  (s/Set. (map analyze x)))

(define-function analyze-variable
  [x]
  (s/Variable. (namespace x) (name x)))

(define-function analyze-quoted
  [x]
  (s/QuotedValue. (analyze x)))

(define-function analyze-definition
  ([name] (s/Definition. name nil (hash-map)))
  ([name value] (s/Definition. name value (hash-map)))
  ([name value meta] (s/Definition. name value meta)))

(define-function analyze-if
  ([pred alt]
   (s/If. (analyze pred) (analyze alt) nil))
  ([pred alt conse]
   (s/If. (analyze pred) (analyze alt) (analyze conse))))

(define-function analyze-lambda
  [args]
  (let [x (first args)]
    (cond (vector? x) (s/Lambda. (list (analyze x)) (list (map analyze (rest args))))
          (list? x)
            (s/Lambda.
              (map analyze (map first args))
              (map analyze (map second args)))
          :else
            (throw (js/Error. "the second element of a Lambda expression should be an arguments vector or a list of bodies")))))

(define-function analyze-block
  [body]
  (s/Block. body))

(define-function analyze-thrown-exception
  [expression]
  (s/ThrownException. expression))

(define-function analyze-property-accessor
  [obj property]
  (s/PropertyAccessor. obj property))

(define-function analyze-property-assignment
  [obj property value]
  (s/PropertyAssignment. obj property value))

(define-function analyze-method-application
  [obj method args]
  (s/MethodApplication. obj method args))

(define-function analyze-class-instantiation
  [class args]
  (s/ClassInstatiation. class args))

(define-function analyze-application
  [fn args]
  (s/Application. fn args))

(define-function analyze [exp]
  (let [exp_ (macroexpand exp)]
    (cond (nil? exp) (analyze-nil exp)
          (number? exp) (analyze-number exp)
          (boolean? exp) (analyze-boolean exp)
          (string? exp) (analyze-string exp)
          (date? exp) (analyze-date exp)
          (regexp? exp) (analyze-regexp exp)
          (keyword? exp) (analyze-keyword exp)
          (map? exp) (analyze-map exp)
          (vector? exp) (analyze-vector exp)
          (set? exp) (analyze-set exp)
          (variable? exp) (analyze-variable exp)
          (quoted? exp) (apply analyze-quoted (rest exp))
          (definition? exp) (apply analyze-definition (rest exp))
          (if? exp) (apply analyze-if (rest exp))
          (lambda? exp) (analyze-lambda (rest exp))
          (block? exp) (apply analyze-block (rest exp))
          (assignment? exp) (apply analyze-assignment (rest exp))
          (thrown-exception? exp) (apply analyze-thrown-exception (rest exp))
          (property-accessor? exp) (apply analyze-property-accessor (rest exp))
          (property-assignment? exp) (apply analyze-property-assignment (rest exp))
          (method-application? exp) (apply analyze-method-application (rest exp))
          (class-instantiation? exp) (apply analyze-class-instantiation (rest exp))
          (application? exp) (apply analyze-application exp)
          :else 
            (do
              (println exp)
              (throw (str "analyz: invalid form: '" exp "'"))))))
