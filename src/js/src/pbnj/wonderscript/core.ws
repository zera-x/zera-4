; vim: ft=clojure
(module 'pbnj.core)

; aliases
(define list? isList)
(define map? isMap)
(define vector? isVector)
(define set? isSet)
(define collection? isCollection)
(define seq? isSeq)
(define sequential? isSequential)
(define associative? isAssociative)
(define counted? isCounted)
(define indexed? isIndexed)
(define reduceable? isReduceable)
(define seqable? isSeqable)
(define reversible? isReversible)

(define symbol? isSymbol)
(define keyword? isKeyword)

; js types
(define number? isNumber)
(define string? isString)
(define boolean? isBoolean)
(define nil? isNull)
(define undefined? isUndefined)
(define date? isDate)
(define error? isError)
(define regexp? isRegExp)
(define function? isFunction)
(define object? isObject)
(define arguments? isArguments)
(define element? isElement)
(define map-object mapObject)
(define map-indexed mapIndexed)

; arrays
(define array? isArray)
(define arraylike? isArrayLike)
(define ->array toArray)
(define into-array intoArray)

(define odd? isOdd)
(define even? isEven)

(define subset? isSubset)
(define superset? isSuperset)

(define empty? isEmpty)

; other mori functions
(define has? hasKey)
(define has-key? hasKey)
(define hash-map hashMap)
(define sorted-set sortedSet)
(define get-in getIn)
(define assoc-in assocIn)
(define update-in updateIn)
(define reduce-kv reduceKV)
(define take-while takeWhile)
(define drop-while dropWhile)
(define sort-by sortBy)
(define partition-by partitionBy)
(define group-by groupBy)
(define prim-seq primSeq)
(define ->ws toClj)
(define ->js toJs)

(define + add)
(define - sub)
(define * mult)
(define / div)

(define-macro comment [&forms] nil)

(define-macro let [bindings &body]
  (cond (not (vector? bindings)) (throw "let bindings should be a vector"))
  (cons 'do
        (concat (map (pair bindings) (lambda [pair] (list 'define (pair 0) (pair 1))))
                body)))

(define-macro if
  ([pred conse] (list 'cond pred conse))
  ([pred conse alt] (list 'cond pred conse :else alt))) 

(define-macro if-not
  ([pred conse] (list 'cond (list 'not pred) conse))
  ([pred conse alt] (list 'cond (list 'not pred) conse :else alt)))

(define-macro unless [pred &acts]
  (list 'cond (list 'not pred) (cons 'do acts)))

(define-macro when [pred &acts]
  (list 'cond pred (cons 'do acts)))

(define-macro or
  ([] nil)
  ([a] a)
  ([&forms]
   (let [or* (first forms)]
     (list 'if or* or* (cons 'or (rest forms))))))

(define-macro and
  ([] true)
  ([a] a)
  ([&forms]
   (let [and* (first forms)]
     (list 'if and* (cons 'and (rest forms)) and*))))

(define-macro define-function
  [name &forms]
  (list 'define name
        (cons 'lambda forms)))

(define-syntax define-once [exp]
  (let [r (rest exp)]
    (list 'if (list 'not (list 'defined? (first r)))
      (list 'define (first r) (second r))
      :defined)))

(define-syntax define-struct [exp]
  (let [name (second exp)
        args (rest (rest exp))]
    (list 'define name (cons 'struct (cons (keyword (str name)) args)))))

(define-syntax not= [exp]
  (list 'not (cons '= (rest exp))))

(define-function add1 [n] (+ 1 n))
(define-function sub1 [n] (- 1 n))

(define-function join
  [col delim]
  (reduce col (lambda [s x] (str s delim x))))

; TODO: make a version of this for the core lib with mori types
(define entries ->array)

; JS Interop / OOP
(read-file "src/pbnj/jess.ws")

(define-macro .-
  [obj prop]
  (list 'pbnj.jess/eval (list 'quote (list '.- obj prop))))

(define-macro .
  ([obj method] (list (list '.- obj method)))
  ([obj method &args] (cons (list '.- obj method) args)))

(define-macro .?
  [obj method &args]
  (list 'if (list '.- obj method) (cons '. (cons obj (cons method args))) nil))

(define-macro new
  [klass &args]
  (list 'pbnj.jess/eval (list 'quote (cons 'new (cons klass args)))))

(define-function ws->jess [exp]
  (cond (or (number? exp) (boolean? exp)) exp
        (string? exp) exp
        (symbol? exp) (list 'pbnj.core.symbol (namespace exp) (name exp))
        (keyword? exp) (list 'pbnj.core.keyword (namespace exp) (name exp))
        (list? exp) (cons 'pbnj.core.list (map exp (lambda [x] (ws->jess x))))
        (vector? exp) (cons 'pbnj.core.vector (map exp (lambda [x] (ws->jess x)))) 
        (map? exp) (cons 'pbnj.core.hashMap (mapcat exp (lambda [xs] [(ws->jess (xs 0)) (ws->jess (xs 1))]))) 
        :else
          (throw (str "invalid expression: " (inspect exp)))))

(define-macro define-class
  [klass &forms]
  (let [nm (symbol (name klass))
        fields (first forms)
        methods (map (rest forms)
                     (lambda [m]
                             (list (first m)
                                   (second m)
                                   (list 'pbnj.wonderscript.eval
                                         (ws->jess (cons 'do (rest (rest m))))))))]
    (list 'define klass
          (list 'pbnj.jess/eval
                (list 'quote (cons 'class (cons nm (cons fields methods))))))))

(define-class Point
  [x y]
  (toString
    [self]
    (str "(" (.- self x) ", " (.- self y) ")")))

(comment
(define-class pbnj.core/PersistentList [h t]
  (first [] this.h)
  (rest  [] this.t)
  (isEmpty [] (and (nil? this.h) (nil? this.t))))

(define pbnj.core.PersistentList/EMPTY (new PersistentList nil nil))

(define-function plist
  [&elements]
  (reduce elements
          (lambda [e] ) ))
)
