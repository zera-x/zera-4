; vim: ft=clojure
(module pbnj.core)

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
        (concat (map (pair bindings)
                     (lambda [pair] (list 'define- (pair 0) (pair 1))))
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

(define-macro define-function-
  [name &forms]
  (list 'define- name
        (cons 'lambda forms)))

(define-macro not= [&values]
  (list 'not (cons '= values)))

(define-function add1 [n] (+ 1 n))
(define-function sub1 [n] (- 1 n))

(define-function join
  [col delim]
  (reduce col (lambda [s x] (str s delim x))))

; TODO: make a version of this for the core lib with mori types
(define entries ->array)

(define-macro define-test [nm &body]
  (list 'define nm
        {:test/name (list 'quote nm)
         :test/fn (cons 'lambda (cons [] (concat body [[(list 'quote nm) :passed]])))}))

(define-macro is [body]
  (list 'if-not body (list 'throw (str "FAILURE: " (inspect body) " is false"))))

(define-macro is-not [body]
  (list 'is (list 'not body)))

(define-function run-test [t] ((:test/fn t)))

(define-function collect-tests [module]
  (filter (into [] (values module)) (lambda [m] (and (map? m) (:test/name m)))))

(define-function run-tests [module]
  (into [] (map (collect-tests module) run-test)))


(define-test test-literals
  (is (= 1 (read-string "1")))
  (is (= -1 (read-string "-1")))
  (is (= 3.14159 (read-string "3.14159")))
  (is (= 3000 (read-string "3_000")))
  (is (= "abc" (read-string "\"abc\"")))
  (is (= :a (read-string ":a")))
  (is (= 'b (read-string "'b")))
  (is (= '(1 2 3) (read-string "'(1 2 3)")))
  (is (= [1 2 3] (read-string "[1 2 3]")))
  (is (= {:a 1 :b 2 :c 3} (read-string "{:a 1 :b 2 :c 3}")))
  (is (= 5000 (read-string "5,000")))
  (is (= 5000 (read-string "5_000")))
  (is (= 5 (read-string "5.000"))))

(define-test test-let
  (is (= [1 2]
         (let [x 1
               y (+ x 1)]
           (is (= x 1))
           (is (= y 2))
           [x y]))))

(define-test test-if
  (is (= 1 (if true 1)))
  (is (= 1 (if true 1 2)))
  (is (= 2 (if false 1 2)))
  (is (= nil (if false 1)))
  (is (= 2 (if nil 1 2))))

(define-test test-if-not
  (is (= 1 (if-not false 1)))
  (is (= 1 (if-not false 1 2)))
  (is (= 2 (if-not true 1 2)))
  (is (= nil (if-not true 1)))
  (is (= 1 (if-not nil 1 2))))

(define-test test-unless
  (is (= 5 (unless false 1 2 3 4 5)))
  (is (= nil (unless true 1 2 3 4 5))))

(define-test test-when
  (is (= 5 (when true 1 2 3 4 5)))
  (is (= nil (when false 1 2 3 4 5))))

(define-test test-or
  (is (or true))
  (is (or false true))
  (is (or false false true))
  (is (or false false false true))
  (is-not (or false))
  (is-not (or false false))
  (is-not (or false false false)))

(define-test test-add
  (is (and true))
  (is (and true true))
  (is (and true true true))
  (is-not (and false))
  (is-not (and false false))
  (is-not (and false false false))
  (is-not (and false true))
  (is-not (and false true true))
  (is-not (and true true false)))

(define-test test-define-function
  (define-function ident [x] x)
  (define-function inc [x] (+ 1 x))
  (is (= 1 (ident 1)))
  (is (= :a (pbnj.core/ident :a)))
  (is (= 4 (inc 3)))
  (is (= 5 (pbnj.core/inc 4))))

(define-test test-define-function-
  (define-function- ident- [x] x)
  (define-function- inc- [x] (+ 1 x))
  ;(is (= :a (pbnj.core/ident- :a)))
  ;(is (= 5 (pbnj.core/+1- 4)))
  (is (= 1 (ident- 1)))
  (is (= 4 (inc- 3))))

(define-test test-not=
  (is (not= 1 2))
  (is (not= :a :b))
  (is (not= 1 :a))
  (is-not (not= 1 1))
  (is-not (not= :a :a))
  (is-not (not= [1 2 3 4] [1 2 3 4])))

(require "src/pbnj/jess.ws")
(require "src/pbnj/wonderscript/compiler.ws")

(use pbnj.core)

(define *sym-count* 0)
(define-function gen-sym 
  ([] (gen-sym "sym"))
  ([prefix]
   (let [sym (symbol (str prefix "-" *sym-count*))]
     (set! *sym-count* (+ 1 *sym-count*))
     sym)))

(define-test test-gen-sym
  (is (symbol? (gen-sym)))
  (is (symbol? (gen-sym "prefix")))
  (is-not (= (gen-sym) (gen-sym)))
  (is-not (= (gen-sym "prefix") (gen-sym "prefix"))))

(define-macro .
  [obj method &args]
  (let [name (gen-sym)]
    (list 'do
          (list 'define name obj)
          (list 'apply-method name (list '.- name method) (into (vector) args)))))

(define-test test-.
  (let [d (new js/Date 2016 10 25)]
    (is (= (. d getMonth) 10))
    (is (= (. d getFullYear) 2016))
    (is (= (. d getDate) 25))
    (is (= (. js/Math abs -3) 3))))

(define-macro .?
  [obj method &args]
  (list 'if (list '.- obj method) (cons '. (cons obj (cons method args))) nil))

(define-test test-.?
  (let [d (new js/Date 2016 10 25)]
    (is (= (.? d getMonth) 10))
    (is (= (.? d missing-method) nil))))

(define-macro define-class [nm fields &methods]
  (let [klass (symbol nm)
        assigns (map-indexed
                  (lambda [i f]
                          (list '.-set! 'this (name f) (list '.- 'arguments i))) fields)
        ctr (cons 'function (cons nm (cons (apply vector fields) assigns)))
        meths (map methods
                   (lambda [meth]
                           (let [n (str (first meth))
                                 args (first (rest meth))
                                 body (rest (rest meth))]
                             (list '.-set! klass (vector "prototype" n)
                                   (list 'fn
                                         (into (vector) (rest args))
                                         (list '.
                                               (cons 'fn (cons [(first args)] (map body ws->jess)))
                                               'apply
                                               'this
                                               (list '. '[this] 'concat (list 'Array.prototype.slice.call 'arguments))))))))
        proto (cons (list '.-set! klass "prototype" (hash-map)) meths)]
    (list 'define nm (list 'pbnj.jess/eval (list 'quote (into (list) (reverse (concat (list 'do) (list ctr) proto (list klass)))))))))

(define-test test-define-class
  (define-class Point
    [x y]
    (toString
      [self]
      (str "(" (.- self x) ", " (.- self y) ")")))
  (let [p (new Point 3 4)]
    (is (= 3 (.- p x)))
    (is (= 4 (.- p y)))
    (is (= "(3, 4)", (str p)))
    (is (= "(3, 4)", (. p toString)))))

(comment
(define-macro define-method
  [nm value args &body]
  )


(define-method distance Point [p1 p2]
  (Math/sqrt (+ (Math/pow (- (point-x p2) (point-x p1)) 2) (Math/pow (- (point-y p2) (point-y p1)) 2)))) 

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
