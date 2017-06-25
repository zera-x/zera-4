; vim: ft=clojure
(module pbnj.core)

(set! *environment* :production)

(define-macro define-function
  "(define-function name doc-string? meta-map? arguments body)
  (define-function name doc-string? meta-map? (arguments body))"
  [name &forms]
  (if (symbol? name)
    nil
    (throw (js/Error. "first argument of define-function should be a symbol")))
  (let [x (first forms)
        y (second forms)]
    (cond (and (string? x) (map? y))
            (list 'define (assoc y :doc x) name (cons 'lambda (rest (rest forms))))
          (string? x)
            (list 'define {:doc x} name (cons 'lambda (rest forms)))
          (map? x)
            (list 'define x name (cons 'lambda (rest forms)))
          (or (vector? x) (list? x))
            (list 'define name (cons 'lambda forms))
          :else
            (throw (js/Error. "after name define-function expects a doc string, an arguments vector, or a list of bodies")))))

(define-macro define-function-
  [name &forms]
  (list 'define :private name (cons 'lambda forms)))

(define-macro comment [&forms] nil)

(define-macro if
  ([pred conse] (list 'cond pred conse))
  ([pred conse alt] (list 'cond pred conse :else alt))) 

(define-macro when [pred &acts]
  (list 'cond pred (cons 'do acts)))

(define-macro case
  "
  (case a
     5 \"a is 5\"
     6 \"a is 6\"
     :else \"a is not 5 or 6\")
  =>
  (cond (= a 5) \"a is 5\"
        (= a 6) \"a is 6\"
        :else \"a is not 5 or 6\")
  "
  [value &rules]
  (cons 'cond (->> (partition 2 rules)
                   (mapcat (lambda [r] [(if (= :else (first r)) :else (list '= value (first r))) (second r)])))))

(define-macro define-
  "define a private variable (it cannot be seen outside of the module scope)."
  ([nm] (list 'define :private nm))
  ([nm value] (list 'define :private nm value))
  ([meta nm value] (list 'define (assoc meta :private true) nm value)))

(define-macro let [bindings &body]
  (cond (not (vector? bindings)) (throw "let bindings should be a vector"))
  (cons 'do
        (concat (map (lambda [pair] (list 'define :private (first pair) (second pair)))
                     (partition 2 bindings))
                body)))

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

(define-macro .?
  ([obj method]
   (list '.? obj method nil))
  ([obj method alt]
   (if (list? method)
     (list 'if (list 'and obj (list '.- obj (first method))) (list '. obj method) alt)
     (list 'if (list 'and obj (list '.- obj method)) (list '. obj method) alt))))

(define-macro ..
  ([x form] (list '. x form))
  ([x form &more] (cons '.. (cons (list '. x form) more))))

(define-macro ..?
  ([x form] (list '.? x form))
  ([x form &more] (cons '..? (cons (list '.? x form) more))))

(define-macro ..-
  ([x form] (list '.- x form))
  ([x form &more] (cons '..- (cons (list '.- x form) more))))

(define-macro defined?
  [sym]
    (let [ns (namespace sym)
                nm (name sym)]
          (list 'if ns
                (list '.- (list 'symbol ns) (list 'symbol nm))
                (list '.? (list 'current-module-scope) (list 'lookup (list 'symbol nm))))))

(define-macro do-times
  [bindings &body]
  (if-not (and (vector? bindings) (= (count bindings) 2))
    (throw "bindings should be a vector with two elements"))
  (let [var (bindings 0)
        init (bindings 1)]
    (list 'loop [var 0]
          (cons 'when
                (cons (list '< var init)
                      (concat body [(list 'again (list '+ var 1))])))
          init)))

(define-macro do-each
  [bindings &body]
  (if-not (and (vector? bindings) (= (count bindings) 2))
    (throw "bindings should be a vector with two elements"))
  (let [var (bindings 0)
        col (bindings 1)
        init (gen-sym "$init")
        col-nm (gen-sym "$col")]
    (list 'let [init col]
    (list 'loop [var (list 'first init) col-nm (list 'rest init)]
          (cons 'when
                (cons var
                      (concat body [(list 'again (list 'first col-nm) (list 'rest col-nm))])))
          init))))

(define-macro while
  [pred &body]
  (list 'loop []
         (cons 'when (cons pred (concat body [(list 'again)])))))

(define-macro until
  [pred &body]
  (list 'loop []
        (cons 'when (cons (list 'not pred) (concat body [(list 'again)])))))

(define-macro ->
  [x &forms]
  (loop [x* x, forms* forms]
    (if (empty? forms*)
      x*
      (let [form (first forms*)
            threaded (if (seq? form)
                       (list (first form) x* (first (rest form)))
                       (list form x*))]
        (again threaded (rest forms*))))))

(define-macro ->>
  [x &forms]
  (loop [x* x, forms* forms]
    (if (empty? forms*)
      x*
      (let [form (first forms*)
            threaded (if (seq? form)
                       (list (first form) (first (rest form)) x*)
                       (list form x*))]
        (again threaded (rest forms*))))))

(define-protocol IDeref
  (deref [self] (.- self value)))

(define-type Atom
  [value]
  IDeref
  (addWatch
    [self k f]
    (.-set! self watches
            (if (.- self watches)
              (conj (.- self watches) [k f])
              (list [k f]))))
  (processWatches
    [self newVal]
    (when (.- self watches)
      (do-each [x (.- self watches)]
        (let [k (x 0)
              f (x 1)]
          (f k self (.- self value) newVal)))))
  (reset
    [self val]
    (. self (processWatches val))
    (.-set! self value val)
    self)
  (swap
    [self f]
    (let [newVal (f (.- self value))]
      (. self (processWatches newVal))
      (.-set! self value newVal)
      self)))

(define-function atom
  "Constructs a new Atom wrapping the given value `x`."
  [x]
  (new Atom x))

(define-function deref
  "
  Dereferences (returning a hidden value) an instance of the `IDef` protocol `x`,
  including Atoms. The reader provides a shorthand of the form:
      
      (define x (atom 1))
      (deref x) => 1
      @x => 1
  "
  [x]
  (.? x deref))

(define-function reset!
  "
  Resets the value of an Atom `x` to `value`, any watches that have
  been added will be processed.

  Example:
      (define x (atom 1))
      (rest! x 2)
      @x => 2
  "
  [x value]
  (.? x (reset value)))

(define-function swap!
  "
  Swaps the the value of `x` with the return value of the function `f`
  which receives the current value of `x` as it's single argument.
  
  Example:
      (define x (atom 1))
      (swap! x (lambda [val] (+ 1 val)))
      (deref x) => 2
  "
  [x f]
  (.? x (swap f)))

(define-function add-watch [x k f]
  (.? x (addWatch k f)))

(define-function add1 [n] (+ 1 n))
(define-function sub1 [n] (- 1 n))

(define- *sym-count* (atom 0))
(define-function gen-sym 
  ([] (gen-sym "sym"))
  ([prefix]
   (let [sym (symbol (str prefix "-" @*sym-count*))]
     (swap! *sym-count* add1)
     sym)))

;; ---------------------------- meta data ------------------------------ ;;

(define current-module
  (lambda [] pbnj.wonderscript/MODULE_SCOPE))

(define current-module-name
  (lambda [] (.- pbnj.wonderscript/MODULE_SCOPE (symbol "@@NAME@@"))))

(define current-module-scope
  (lambda [] (.- pbnj.wonderscript/MODULE_SCOPE (symbol "@@SCOPE@@"))))

(define-macro var
  "Return the named Variable object"
  ([nm]
   (let [ns (namespace nm)
         sname (name nm)]
    (if ns
      (list 'var (symbol sname) (list '.- (symbol ns) "@@SCOPE@@"))
      (list 'or
            (list 'var nm '*scope*)
            (list 'var nm (list '.- (current-module-name) "@@SCOPE@@"))
            (list 'var nm (list '.- 'pbnj.core "@@SCOPE@@"))))))
  ([nm scope]
   (list '..?
         scope
         (list 'lookup (list 'str (list 'quote nm)))
         (list 'getObject (list 'str (list 'quote nm))))))

(define-function meta
  "Return meta data for the given Variable object `x`."
  [x] (.getMeta x))

(define-function with-meta
  "Add meta data `meta` (expects a Map) to the given Variable object `x`."
  [x meta]
  (.withMeta x meta))

(define-function vary-meta
  "Change the meta data for the given Variable object `x`,
  with the function `f` and any required `args`."
  [x f &args]
  (.varyMeta x f args))

(define-macro define-once
  [nm value]
  (list 'cond (list 'not (list 'defined? nm)) (list 'define nm value) :else nm))

(define-macro defined-in-module?
  ([nm] (list '.- 'pbnj.wonderscript/MODULE_SCOPE nm))
  ([mod nm]
   (list '.- mod nm)))

(define-macro define-in-module-once [nm value]
  (list 'cond (list 'not (list 'defined-in-module? nm)) (list 'define nm value) :else nm))

(define-macro doc
  [sym]
  (let [meta (gen-sym "meta")
        src (gen-sym "src")
        doc (gen-sym "doc")
        args (gen-sym "args")
        kind (gen-sym "kind")]
    (list 'let [meta (list '.? (list 'var sym) 'getMeta)]
          (list 'if
                meta
                (list 'let [doc  (list meta :doc)
                            args (list meta :arglists)
                            kind (list 'if (list meta :macro) :macro :function)]
                      (list 'println (list 'str kind))
                      (list 'println (list 'str args))
                      (list 'if doc (list 'println doc)))))))

(define-macro source
  [sym]
  (let [meta (gen-sym "meta")]
    (list 'let [meta (list '.getValue (list 'var sym))]
          (list 'println (list '.toString meta)))))

;; ------------------------------------ testing -------------------------------------- ;;

(define-macro test [nm &body]
  (when (and (defined? *environment*) (or (= *environment* :development) (= *environment* :test)))
    (let [t (cons 'lambda (cons [] (concat body [[(keyword (namespace nm) (name nm)) :passed]])))]
      (list 'if
            (list 'defined? nm)
            (list 'vary-meta (list 'var nm) 'assoc :test t)
            (list 'define {:tag :test, :test t} nm :test)))))

(define :private format-test-results
 (lambda [body]
  (println "body" body)
  (if (list? body)
    (inspect (cons (first body) (map eval (rest body))))
    (inspect body))))

(define-macro is
  ([body]
   (list 'is body (list 'str "FAILURE: " (inspect body) " is false")))
  ([body msg]
   (list 'cond
         (list 'not body) (list 'throw msg))))

(define-macro is-not [body &args]
  (cons 'is (cons (list 'not body) args)))

(define-function module-scope
  "Return the scope of the named module."
  [mod]
  (if (symbol? mod)
    (.- (eval mod) (symbol "@@SCOPE@@"))
    (.- mod (symbol "@@SCOPE@@"))))

(define-function tests
  "Collect all the tests in the given module, if no module us specified
  the tests from the current module are returned."
  ([] (tests (current-module)))
  ([mod]
   (->> (module-scope mod)
        .getObjects
        (map meta)
        (filter :test)
        (map :test))))

(define-macro prove
  "Run the named test"
  [tname]
  (let [v (gen-sym "var")]
    (list 'let [v (list 'var tname)]
         (list 'if v (list '-> (list '.getMeta v) :test 'apply)))))

(define-function prove-module [module]
  (map apply (tests module)))

(test prove
  (test passing-test (is (= 1 1)))
  (test failing-test (is (= 0 1)))
  (is (= (prove passing-test) [:passing-test :passed])))

(test .?
  (let [d (new js/Date 2016 10 25)]
    (is (= (.? d getMonth) 10))
    (is (= (.? d missing-method) nil))
    (.? d (setMonth 11))
    (is (= (.? d (getMonth)) 11))))

(test ..
  (let [xs (array 1 2 3 4 5)]
    (.. "1,2,3,4,5"
        (split ",")
        (map (lambda [x &rest] (* 1 x)))
        (forEach (lambda [x i other] (is (= x (.- xs i))))))))

(test ..?
  (let [xs (array 1 2 3 4 5)]
    (..? "1,2,3,4,5"
         (split ",")
         (map (lambda [x &rest] (* 1 x)))
         (forEach (lambda [x i other]
                       (println "x" x "xs" xs (str "xs[" i "]") (.- xs i))
                       (is (= x (.- xs i)))))))
  (is (= nil (..? "1,2,3,4,5" (split ",") (missing-method 1 2 3 4 5)))))

(test read-string
  (let [n (generate-nat)]
    (is (= n (read-string (str n)))))
  (is (= -1 (read-string "-1")))
  (is (= -1.14 (read-string "-1.14")))
  (is (= 1.14 (read-string "+1.14")))
  (is (= 1 (read-string "+1")))
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

(test comment
  (is (= nil (comment)))
  (is (= nil (comment asdfasdf asfasdf sfasdfasd asfasdfasd))))

(test let
  (is (= [1 2]
         (let [x 1
               y (+ x 1)]
           (is (= x 1))
           (is (= y 2))
           [x y]))))

(test if
  (is (= 1 (if true 1)))
  (is (= 1 (if true 1 2)))
  (is (= 2 (if false 1 2)))
  (is (= nil (if false 1)))
  (is (= 2 (if nil 1 2))))

(define-macro if-not
  ([pred conse] (list 'cond (list 'not pred) conse))
  ([pred conse alt] (list 'cond (list 'not pred) conse :else alt)))

(test if-not
  (is (= 1 (if-not false 1)))
  (is (= 1 (if-not false 1 2)))
  (is (= 2 (if-not true 1 2)))
  (is (= nil (if-not true 1)))
  (is (= 1 (if-not nil 1 2))))

(define-macro unless [pred &acts]
  (list 'cond (list 'not pred) (cons 'do acts)))

(test unless
  (is (= 5 (unless false 1 2 3 4 5)))
  (is (= nil (unless true 1 2 3 4 5))))

(test when
  (is (= 5 (when true 1 2 3 4 5)))
  (is (= nil (when false 1 2 3 4 5))))

(test or
  (is (or true))
  (is (or false true))
  (is (or false false true))
  (is (or false false false true))
  (is-not (or false))
  (is-not (or false false))
  (is-not (or false false false)))

(test and
  (is (and true))
  (is (and true true))
  (is (and true true true))
  (is-not (and false))
  (is-not (and false false))
  (is-not (and false false false))
  (is-not (and false true))
  (is-not (and false true true))
  (is-not (and true true false)))

(test define-function
  (define-function ident [x] x)
  (define-function inc [x] (+ 1 x))
  (is (= 1 (ident 1)))
  (is (= :a (pbnj.core/ident :a)))
  (is (= 4 (inc 3)))
  (is (= 5 (pbnj.core/inc 4))))

(test define-function-
  (define-function- ident- [x] x)
  (define-function- inc- [x] (+ 1 x))
  ;(is (= :a (pbnj.core/ident- :a)))
  ;(is (= 5 (pbnj.core/+1- 4)))
  (is (= 1 (ident- 1)))
  (is (= 4 (inc- 3))))

(define-macro not= [&values]
  (list 'not (cons '= values)))

(test not=
  (is (not= 1 2))
  (is (not= :a :b))
  (is (not= 1 :a))
  (is-not (not= 1 1))
  (is-not (not= :a :a))
  (is-not (not= [1 2 3 4] [1 2 3 4])))

(define-function join
  [col delim]
  (let [joiner (lambda [s x] (str s delim x))]
    (reduce joiner col)))

(test gen-sym
  (is (symbol? (gen-sym)))
  (is (symbol? (gen-sym "prefix")))
  (is-not (= (gen-sym) (gen-sym)))
  (is-not (= (gen-sym "prefix") (gen-sym "prefix"))))

(define-function fraction [n]
  (- n (. js/Math floor n)))

(define-function sign [n]
  (if (number? n) (. js/Math sign n) 0))

(define-function positive? [n]
  (= n (. js/Math abs n)))

(define-function negative? [n]
  (not= n (. js/Math abs n)))

(define-function integer? [n] (= 0 (fraction n)))

(define-function natural? [n]
  (and (integer? n) (positive? n)))

(test natural?
  (is (natural? 0))
  (is (natural? 1))
  (is (natural? 34))
  (is (natural? 21412412341234123463456435437456))
  (is-not (natural? -1))
  (is-not (natrual? 1.1)))

(define-function generate-int
  ([] (generate-int -100 100))
  ([min max]
   (let [a (. js/Math ceil min)
         b (. js/Math ceil max)]
     (+ a (. js/Math floor (* (. js/Math random) (- b a)))))))

(test generate-int
  (do-times [n 100]
    (is (integer? (generate-int)))))

(define-function generate-nat
  ([] (generate-int 0 100))
  ([min max]
   (let [a (. js/Math abs min)]
    (if (> a max) (throw "The absolute value of min should be less than max"))
    (generate-int a max))))

(test generate-nat
  (do-times [n 20]
    (let [m (generate-nat)]
      (is (natural? (generate-nat))))))

(define-function generate-float
  ([] (generate-float 0 100))
  ([min max]
   (+ (generate-int min max) (* (. js/Math random (- max min))))))

(test generate-float
  (do-times [n 20]
    (is (number? (generate-float)))))

(define-function generate-str
  ([] (generate-str 1 20))
  ([min max]
   (let [n (generate-nat min max)
         xs (take n (iterate (partial generate-nat 0x20 0x4000) (generate-nat 0x20 0x4000)))]
     (apply-method js/String (.- js/String fromCodePoint) xs))))

(test generate-string
  (do-times [n 20]
    (is (string? (generate-string)))))

(define-function generate-keyword
  ([] (generate-keyword 1 20))
  ([min max]
   (let [ns? (> (generate-int 0 2) 0)
         nm (generate-str min max)
         ns (generate-str min max)]
     (if ns?
      (keyword ns nm)
      (keyword nm)))))

(test generate-keyword
  (do-times [n 20]
    (is (keyword? (generate-keyword)))))

(define-function generate-symbol
  ([] (generate-keyword 1 20))
  ([min max]
   (let [ns? (> (generate-int 0 2) 0)
         nm (generate-str min max)
         ns (generate-str min max)]
     (if ns?
      (symbol ns nm)
      (symbol nm)))))

(test generate-symbol
  (do-times [n 20]
    (is (symbol? (generate-symbol)))))

(define-function maybe
  ([value just]
   (maybe value just nil))
  ([value just none]
   (if (nil? value) none (just value))))

(define-function either
  ([value left]
   (if (nil? value) (left) nil))
  ([value left right]
   (if (nil? value) (left) (right value))))

(define-function identity [x] x)

(define-function always [x] (lambda [] x))

;(define-function str
;  ([] "")
;  ([x] (str x))
;  ([x &more]
;   (str x (reduce (lambda [s x] (str s x)) more))))

(if (= *platform* :nodejs)
  (define-function print [&vals] (. process/stdout (write (apply str vals)))))
