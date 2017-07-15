(module pbnj.core)

; TODO: implement destructure (see https://github.com/clojure/clojure/blob/clojure-1.9.0-alpha14/src/clj/clojure/core.clj#L4341)

(define-protocol Exception)

(define-type MacroException
  [message]
  Exception)

(define-macro define-function
  "(define-function name doc-string? meta-map? arguments body)
  (define-function name doc-string? meta-map? (arguments body))"
  [name &forms]
  (if (symbol? name)
    nil
    (throw (MacroException. "first argument of define-function should be a symbol")))
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
            (throw
              (MacroException.
                "after name define-function expects a doc string, an arguments vector, or a list of bodies")))))

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
  "(case a
     5 \"a is 5\"
     6 \"a is 6\"
     :else \"a is not 5 or 6\")"
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

(define-macro do-to
  "Take an object `x` and perform (presumably) mutation operations `forms` on it.
  The expression evaluates to the object.
  
      Example:
          (do-to person
            (.setFirstName \"John\")
            (.setLastName \"Smith\")
            (.setAge 21))"
  {:added "1.0"}
  [x &forms]
  (let [obj (gen-sym "$obj")]
    (cons 'let
            (cons [obj x]
                  (concat (map (lambda [form] (cons (first form) (cons obj (rest form)))) forms)
                          (list obj))))))

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

(define-function prototype
  [x]
  (cond
    (.-prototype x) (.-prototype x)
    (.-__proto__ x) (.-__proto__ x)
    :else
      nil))

(define-function constructor
  [x]
  (.- (prototype x) constructor))

(define-function isa?
  [x klass]
  (.? x (isa klass) false))

(define-function class
  [x]
  (.? x class (eval (symbol "js" (class-name x)))))

(define-protocol IDeref
  (deref [self] (.- self value)))

(define-protocol IRef
  (setValidator
    [self fn]
    (.-set! self validator fn)
    self)
  (addWatch
    [self k f]
    (.-set! self watches
            (if (.- self watches)
              (assoc (.- self watches) k f)
              {k f}))
    self)
  (removeWatch
    [self k]
    (.-set! self watches
            (if (.-watches self)
              (dissoc (.-watches self) k)))
    self)
  (processWatches
    [self newVal]
    (when (.- self watches)
      (do-each [x (.- self watches)]
        (let [k (x 0)
              f (x 1)]
          (f k self (.- self value) newVal))))
    self))

(define-protocol IMeta
  (getMeta [self] (.-meta self))
  (withMeta [self m] (.-set! self meta m))
  (varyMeta
    [self f &args]
    (.withMeta (apply f (cons (.-meta self) args)))))

(define-type Atom
  [value meta validator]
  IMeta
  IDeref
  IRef
  (reset
    [self val]
    (if (.-validator self)
      (if-not (.validator self val)
        (throw (js/Error. (str "'" (inspect val) "' is not a valid value")))))
    (. self (processWatches val))
    (.-set! self value val)
    self)
  (swap
    [self f]
    (let [newVal (f (.- self value))]
      (if (.-validator self)
        (if-not (.validator self newVal)
          (throw (js/Error. (str "'" (inspect newVal) "' is not a valid value")))))
      (. self (processWatches newVal))
      (.-set! self value newVal)
      self)))

(define-function atom
  "Constructs a new Atom wrapping the given value `x`."
  {:added "1.0"}
  [x &options]
  (if (empty? options)
    (Atom. x {} nil)
    (let [opts (partition 2 options)
          meta (first (filter (lambda [x] (= :meta (first x))) opts))
          validator (first (filter (lambda [x] (= :validator (first x))) opts))]
      (cond (and meta validator)
              (Atom. x (second meta) (second validator))
            validator
              (Atom. x {} (second validator))
            meta
              (Atom. x (second meta) nil)
            :else
              (Atom. x {} nil)))))

(define-function atom?
  [x]
  (isa? x Atom))

(define-function deref
  "Dereferences (returning a hidden value) an instance of the `IDef` protocol `x`,
  including Atoms. The reader provides a shorthand of the form:
      
  Example:
      (define x (atom 1))
      (deref x) => 1
      @x => 1"
  {:added "1.0"}
  ([x]
   (.? x deref))
  ([x timeout-ms timeout-val]
   (.? x (deref timeout-ms timeout-val))))

(define-function reset!
  "Resets the value of an Atom `x` to `value`, any watches that have
  been added will be processed.

  Example:
      (define x (atom 1))
      (rest! x 2)
      @x => 2"
  {:added "1.0"}
  [x value]
  (.? x (reset value)))

(define-function swap!
  "Swaps the the value of `x` with the return value of the function `f`
  which receives the current value of `x` as it's single argument.
  
  Example:
      (define x (atom 1))
      (swap! x (lambda [val] (+ 1 val)))
      (deref x) => 2"
  {:added "1.0"}
  [x f]
  (.? x (swap f)))

(define-function add-watch
  "Add watch function to the atom `x` with a key `k` (that can be used for traces).
  The watch function will be proccessed each time the atom value changes. When
  the watch function is proccessed it is passed 4 arguments, the key, the atom,
  the current value, and the new value.

  Example:
      (define x (atom 1))
      (add-watch x :times-are-a-changin
        (lambda [k a old new]
                (println (str \"processing \" k))
                (println (str \"old value: \" (inspect old)))
                (println (str \"new value: \" (inspect new)))))"
  {:added "1.0"}
  [x k f]
  (.? x (addWatch k f)))

(define-function remove-watch
  "Removes a watch (set by add-watch) from a reference"
  {:added "1.0"}
  [x k]
  (.? x (removeWatch k)))

(define-function compare-and-set!
  "Atomically sets the value of atom to newval if and only if the
  current value of the atom is identical to oldval. Returns true if
  set happened, else false."
  {:added "1.0"}
  [x old new]
  (if (identical? @x old)
    (do
      (reset! x new)
      true)
    false))

(define-function set-validator!
  {:added "1.0"}
  [x fn]
  (.? x (setValidator fn)))

(define-protocol IPending
  (realized?
    [self]
    (.-value self)))

(define-function realized?
  [x]
  (.? x realized?))

(define-type Delay
  [fn value]
  IDeref
  IPending
  (deref
    [self]
    (if-not (.-value self)
      (.-set! self value ((.-fn self))))
    (.-value self)))

(define-macro delay
  [&body]
  (list 'Delay. (cons 'lambda (cons [] body)) nil))

(define-function add1 [n] (+ 1 n))
(define-function sub1 [n] (- 1 n))

(do
  (define- sym-count (atom 0))
  (define-function gen-sym 
    "Generate a unique symbol with an optional prefix (used for symbol generation in macros)."
    {:added "1.0"}
    ([] (pbnj.core/gen-sym "$sym")) ;; FIXME: this should work without fully qualified name
    ([prefix]
     (let [sym (symbol (str prefix "-" @sym-count))]
       (swap! sym-count add1)
       sym))))

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

(define-function var-set
  [x value]
  (.? x (set value)) x)

(define-function var-get
  [x]
  (.? x get))

(define-function macro?
  [x]
  (.? x isMacro))

(define-function var?
  [x]
  (isa? x Var))

(define-function class?
  [x]
  (cond (var? x)
          (.isClass x)
        (and (function? x) (.-getMeta x))
          ((.getMeta x) :type)
        :else false))

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
  "Retreive documentation for the given symbol"
  [sym]
  (let [v (gen-sym "$var")
        meta (gen-sym "$meta")
        src (gen-sym "$src")
        doc (gen-sym "$doc")
        args (gen-sym "$args")
        kind (gen-sym "$kind")]
    (list 'let [v (list 'var sym)
                meta (list '.? v 'getMeta)]
          (list 'if
                v
                (list 'let [doc  (list meta :doc)
                            kind (list 'cond (list meta :macro) "Macro"
                                             (list meta :type) "Type"
                                             (list meta :protocol) "Protocol"
                                             (list meta :special-form) "Special Form"
                                             :else "Function")
                            args (list meta :arglists)]
                      (list 'println "----------------------")
                      (list 'println (list 'str kind))
                      (list 'unless (list 'or (list 'not args) (list '= kind "Protocol"))
                        (list 'println (list 'str args)))
                      (list 'when (list 'or (list '= kind "Type") (list '= kind "Protocol"))
                        (list 'println "Methods")
                        (list 'println
                              (list 'map
                                    '(lambda [x] [(.methodName x) (.arglists x)])
                                    (list '.instanceMethods (list 'deref v)))))
                      (list 'if doc (list 'println doc))
                      (list 'when (list meta :see-also)
                            (list 'println)
                            (list 'println "See Also:")
                            (list 'do-each ['cite (list meta :see-also)]
                                (list 'println 'cite))))) nil)))

(define-macro source
  [sym]
  (let [meta (gen-sym "meta")]
    (list 'let [meta (list 'deref (list 'var sym))]
          (list 'println (list '.toString meta)))))

;; ------------------------------------ testing -------------------------------------- ;;

(define-macro define-test [nm &body]
  (when (and (defined? *mode*) (or (= *mode* :development) (= *mode* :test)))
    (let [t (cons 'lambda (cons [] (concat body [[(keyword (namespace nm) (name nm)) :passed]])))]
      (list 'if
            (list 'defined? nm)
            (list 'vary-meta (list 'var nm) 'assoc :test t)
            (list 'define {:tag :test, :test t} nm :test)))))

(define :private format-test-results
 (lambda [body]
  ;(println "body" body)
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
  (let [v (gen-sym "$var")
        f (gen-sym "$fn")]
    (list 'let [v (list 'var tname)
                f (list 'if v (list '-> (list '.getMeta v) :test))]
         (list 'if f (list 'apply f)))))

(define-function prove-module [module]
  (map apply (tests module)))

;; Special Forms

(define
  {:doc "Takes a set of test/expression pairs. It evaluates each test one at a
        time.  If a test returns logical true, cond evaluates and returns
        the value of the corresponding expr and doesn't evaluate any of the
        other tests or exprs. (cond) returns nil.

        Example:
          (cond (< a 1) :less-than
                (> a 1) :greater-than
                :else   :equal)"
   :special-form true
   :see-also
    '(pbnj.core/if
      pbnj.core/when
      pbnj.core/unless
      pbnj.core/if-not
      pbnj.core/or
      pbnj.core/and)
   :added "1.0"}
  cond)

(define
  {:doc "A function (lambda) expression, not a true function in the strictest sense (it does allow imperative programming, i.e. side-effects)
        but they are true closures, in that any variables that are defined in a higher scope or passed as parameters are available
        to the function at execution time.

        Example:
            (lambda [x] x) ;; the identity function
            (lambda [x] (lambda [] x)) ;; the returning function will always return the first value"
   :special-form true
   :see-also
    '("https://en.wikipedia.org/wiki/Functional_programming"
      "https://en.wikipedia.org/wiki/Imperative_programming"
      "https://en.wikipedia.org/wiki/Closure_(computer_programming)"
      pbnj.core/identity
      pbnj.core/always
      pbnj.core/define-function
      pbnj.core/define-macro
      pbnj.core/define-generic
      pbnj.core/define-method)
   :added "1.0"}
  lambda)

(define-macro if-not
  ([pred conse] (list 'cond (list 'not pred) conse))
  ([pred conse alt] (list 'cond (list 'not pred) conse :else alt)))

(define-macro unless [pred &acts]
  (list 'cond (list 'not pred) (cons 'do acts)))

(define-macro not= [&values]
  (list 'not (cons '= values)))

(define-function fraction [n]
  (- n (.floor js/Math n)))

(define-function sign [n]
  (if (number? n) (.sign js/Math n) 0))

(define-function positive? [n]
  (= n (.abs js/Math n)))

(define-function negative? [n]
  (not= n (.abs js/Math n)))

(define-function integer? [n] (and (number? n) (= 0 (fraction n))))

(define-function natural? [n]
  (and (integer? n) (positive? n)))

(define-function generate-int
  ([] (generate-int -100 100))
  ([min max]
   (let [a (.ceil js/Math min)
         b (.ceil js/Math max)]
     (+ a (.floor js/Math (* (.random js/Math) (- b a)))))))

(define-function generate-nat
  ([] (generate-int 0 100))
  ([min max]
   (let [a (.abs js/Math min)]
    (if (> a max) (throw "The absolute value of min should be less than max"))
    (generate-int a max))))

(define-function generate-float
  ([] (generate-float 0 100))
  ([min max]
   (+ (generate-int min max) (* (.random js/Math (- max min))))))

(define-function generate-str
  ([] (generate-str 1 20))
  ([min max]
   (let [n (generate-nat min max)
         xs (take n (iterate (partial generate-nat 0x20 0x4000) (generate-nat 0x20 0x4000)))]
     (->> xs
          (map (lambda [x] (.fromCodePoint js/String x)))
          (reduce str)))))

(define-function generate-keyword
  ([] (generate-keyword 1 20))
  ([min max]
   (let [ns? (> (generate-int 0 2) 0)
         nm (generate-str min max)
         ns (generate-str min max)]
     (if ns?
      (keyword ns nm)
      (keyword nm)))))

(define-function generate-symbol
  ([] (generate-keyword 1 20))
  ([min max]
   (let [ns? (> (generate-int 0 2) 0)
         nm (generate-str min max)
         ns (generate-str min max)]
     (if ns?
      (symbol ns nm)
      (symbol nm)))))

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

(define-function raise
  [e]
  (lambda [] (throw e)))

(define-function identity [x] x)

(define-function always [x] (lambda [] x))

(define-macro define-generic
  "Defines a generic function

  Example:
      (define-generic t string?)
      (define-method t false [_] :not-string)
      (define-method t true [_] :string)"
  {:added "1.0"}
  ([name fn]
   (list 'define-generic name nil fn))
  ([name doc fn]
   (list 'define
         {:generic-function true :doc doc :methods {}}
         name
         (list 'lambda ['&args]
               (list 'let ['meths (list '-> (list 'var name) '.getMeta :methods)]
                     (list 'if (list 'empty? 'meths)
                           (list 'throw (str "There are no methods for the generic function '" name "'")))
                     (list 'let ['meth (list 'get 'meths (list 'apply fn 'args))]
                           (list 'if-not 'meth
                                 (list 'throw (str "there is no method for the given arguments " (list 'inspect 'args))))
                           (list 'apply 'meth 'args)))))))

(define-macro define-method
  "Defines a method for a generic function

  Example:
      (define-generic t string?)
      (define-method t false [_] :not-string)
      (define-method t true [_] :string)"
  {:added "1.0"}
  [name val args &body]
  (list '.varyMeta (list 'var name)
        (list 'lambda '[m]
              (list 'let ['meths (list 'get 'm :methods)
                          'fn (cons 'lambda (cons args body))]
                    (list 'assoc 'm :methods (list 'assoc 'meths val 'fn))))))

(define-macro nodejs?
  "If `*platform*` is `:nodejs` wrap code in a `do` block
  in place otherwise return `nil`."
  {:added "1.0"}
  [&forms]
  (if (= *platform* :nodejs)
    (cons 'do forms)))

(define-macro browser?
  "If `*platform*` is `:browser` wrap code in a `do` block
  in place otherwise return `nil`."
  {:added "1.0"}
  [&forms]
  (if (= *platform* :browser)
    (cons 'do forms)))

(define-macro javascript?
  "If `*target-language*` is `:javascript` wrap code in a `do` block
  in place otherwise return `nil`."
  {:added "1.0"}
  [&forms]
  (if (= *target-language* :javascript)
    (cons 'do forms)))

(define-macro for-platform
  [&forms]
  (cons 'case (cons '*platform* forms)))

(define-macro for-language
  [&forms]
  (cons 'case (cons '*target-language* forms)))

(define-function say
  "String concatenate `vals` and print to console
  with a new line at the end (depending on platform)."
  {:platforms #{:javascript}
   :added "1.0"}
  [&vals]
  (javascript?
    (console/log (apply str vals))))

(define-function broken
  []
  (throw "this is broken")
  1
  2
  3
  4)

(define-function broken1
  []
  (broken))

(define-function print
  "String concatenate `vals` and print to console
  with a new line at the end (depending on platform)."
  {:platforms #{:nodejs :browser}
   :added "1.0"}
  [&vals]
  (nodejs?
    (.write process/stdout (apply str vals)))
  (browser?
    (console/log (apply str vals))))

(nodejs?

  (define
    {:doc ""
     :platforms #{:nodejs}
     :added "1.0"}
    *operating-system* process/platform)
  
  (define 
    {:doc "A map of environment variables"
     :platforms #{:nodejs}
     :added "1.0"}
    *env* (object->map process/env))

  (define
    {:doc "A vector of command line arguments"
     :platforms #{:nodejs}
     :added "1.0"}
    *argv* (array->vector (.slice process/argv 2)))
)

(javascript?

  (define-function lines
    [s]
    (-> (.split s \newline) array->list))
  
  (define-function split
    [s delim]
    (-> (.split s delim) array->list))
  
  (define-function join
    [col delim]
    (let [joiner (lambda [s x] (str s delim x))]
      (reduce joiner col)))
  
  (define-function replace
    ([s pat]
     (replace s pat ""))
    ([s pat val]
     (if (nil? s)
       ""
       (.replace s pat val))))
)

(define-function json
  [x]
  (.parse js/JSON x))

(define-macro promise
  [binds &forms]
  (cons 'js/Promise. (cons 'lambda (cons binds forms))))

(define-macro >>
  [x &forms]
  (cons '..
    (cons x
          (map
            (lambda [form] (list 'then (list 'lambda '[x] (list form 'x))))
            forms))))
