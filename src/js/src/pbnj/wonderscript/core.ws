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
(define has-key hasKey)
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

(define-function add1 [n] (+ 1 n))
(define-function sub1 [n] (- 1 n))

(define-syntax comment [exp] nil)

(define-syntax let [exp]
  (do
    (define bindings (pair (second exp)))
    (cons 'do (concat (map bindings (lambda [pair] (list 'define (pair 0) (pair 1))))
                      (rest (rest exp))))))

(define-syntax on [exp] true)
(define-syntax off [exp] false)
(define-syntax yes [exp] true)
(define-syntax no [ex] false)

(define-syntax if [exp]
  (let [r (rest exp)
        pred (first r)
        conse (second r)
        alt (second (rest r))]
    (cond alt (list 'cond pred conse :else alt)
          :else (list 'cond pred conse))))

(define-syntax if-not [exp]
  (let [r (rest exp)
        pred (first r)
        conse (second r)
        alt (second (rest r))]
    (cond alt (list 'cond (list 'not pred) conse :else alt)
          :else (list 'cond (list 'not pred) conse))))

(define-syntax unless [exp]
  (let [pred (second exp)
        conse (second (rest exp))]
    (list 'cond (list 'not pred) conse)))

(define-syntax when [exp]
  (let [pred (second exp)
        conse (second (rest exp))]
    (list 'cond pred conse)))

(define-syntax or [exp]
  (list 'if (second exp) (list 'if (second (rest exp)) true true) false))

(define-syntax and [exp]
  (list 'if (second exp) (list 'if (second (rest exp)) true false) false))

(define-syntax define-function [exp]
  (let [r (rest exp)]
    (list 'define (first r) (cons 'lambda (cons (second r) (rest (rest r)))))))

(define-syntax define-once [exp]
  (let [r (rest exp)]
    (list 'if (list 'not (list 'defined? (first r)))
      (list 'define (first r) (second r))
      :defined)))

(define-syntax define-struct [exp]
  (let [name (second exp)
        args (rest (rest exp))]
    (list 'define name (cons 'struct (cons (keyword (str name)) args)))))


(define-function join
  [col delim]
  (reduce col
          (lambda [s x]
                  (str s delim x)))))

; TODO: make a version of this for the core lib with mori types
(define entries ->array)

(define-function pprint [exp]
  (cond (number? exp) (str exp)
        (string? exp) (str "\"" exp "\"")
        (symbol? exp) (str exp)
        (keyword? exp) (str exp)
        (boolean? exp) (cond exp "true" :else "false")
        (nil? exp) "nil"
        (date? exp) (str "#inst \"" exp "\"")
        (regexp? exp) (str "#\"" exp "\"")
        (map? exp)
          (str "{" (join (map (apply concat (entries exp)) pprint) " ") "}")
        (collection? exp)
          (let [delims
                (cond (vector? exp) ["[" "]"]
                  (set? exp) ["#{" "}"]
                  :else ["'(" ")"])]
             (str (delims 0) (join (map exp pprint) " ") (delims 1)))
        :else (str exp))) 
