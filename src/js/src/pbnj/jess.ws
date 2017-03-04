(println (str *source* ":" *line*))
; vim: ft=clojure
(module pbnj.jess)

(define-function emit-nil [exp] "null")
(define-function emit-number [exp] (str exp))
(define-function emit-boolean [exp] (if exp "true" "false"))

(define-function pbnj.jess/emit-symbol [exp]
  (let [nm (name exp)
        ns (namespace exp)]
    (if (nil? ns) nm (str ns "." nm))))

(define-function emit-string [exp] (str "\"" exp "\""))

(define-function emit-expression [exp] (str "(" (pbnj.jess/compile (second exp)) ")"))

(define-function emit-block [exp] (str (join (map exp pbnj.jess/compile) ";") ";"))

(define-function emit-if-else [exp]
  (let [size (count exp)]
    (unless (and (list? exp) (<= 3 size) (odd? size))
            (throw "malformed expression: an if-else should be a list with at least 3 elements"))
    (reduce (pair (rest exp))
      (lambda [memo exp]
              (cond (nil? memo)
                      (str "if(" (pbnj.jess/compile (exp 0)) "){" (pbnj.jess/compile (exp 1)) "}")
                    (= :else (exp 0))
                      (str memo "else{" (pbnj.jess/compile (exp 1)) "}")
                    :else
                      (str memo "else if(" (pbnj.jess/compile (exp 0)) "){" (pbnj.jess/compile (exp 1)) "}"))) nil)))

(define-function emit-if [exp]
  (let [pred (second exp)
        conse (first (rest (rest exp)))
        alt (second (rest (rest exp)))]
    (str "(" (pbnj.jess/compile pred) ")?(" (pbnj.jess/compile conse) "):(" (pbnj.jess/compile alt) ")")))

(define-function emit-existential [exp]
  (let [value (pbnj.jess/compile (second exp))]
    (str "((typeof " value "!==\"undefined\")&&(" value "!==null))")))

(define-function emit-instanceof [exp]
  (str "(" (pbnj.jess/compile (second exp)) " instanceof " (pbnj.jess/compile (first (rest (rest exp)))) ")"))

(define-function emit-typeof [exp]
  (str "(typeof " (pbnj.jess/compile (second exp)) ")"))

(define-function emit-label [exp]
  (str (name (second exp)) ":"))

(define-function pbnj.jess/definition? [exp]
  (let [tag (first exp)]
    (or (= tag 'var) (= tag 'let) (= tag 'const))))

(define-function emit-definition [exp]
  (let [size (count exp)]
    (cond (= size 2)
            (str (first exp) " " (second exp) ";")
          (= size 3)
            (str (first exp) " " (second exp) "=" (pbnj.jess/compile (first (rest (rest exp)))) ";")
          :else
            (throw "malformed expression: a definition should be a list of 2 or 3 elements"))))

(define-function emit-argument-list [args]
  (if (empty? args) "()"
      (str "(" (join (map args compile) ",") ")")))

(define-function emit-function [exp]
  (let [ident (second exp)]
    (cond (sequential? ident)
            (str (first exp) (emit-argument-list (second exp)) "{" (emit-block (rest (rest exp))) "}")
          (symbol? ident)
            (str (first exp) " " (second exp) (emit-argument-list (second (rest exp))) "{" (emit-block (rest (rest (rest exp)))) "}")
          :else
            (throw "malformed function expression"))))

(define-function emit-peren [exp]
  (str "(" (pbnj.jess/compile exp) ")"))

(define-function emit-statement [exp]
  (let [size (count exp)]
    (cond (= size 1)
            (str (first exp) ";")
          (= size 2)
            (str (first exp) " " (pbnj.jess/compile (second exp)) ";")
          :else
            (throw "a statement should be a list of 1 or 2 elements"))))

(define-function emit-return 
  ([] "return;")
  ([val]
   (if (pbnj.jess/definition? val)
     (str (emit-definition val) "return " (second val) ";")
     (str "return " (pbnj.jess/compile val) ";"))))

(define-function emit-colon-statement [exp]
  (let [size (count exp)]
    (cond (= size 1)
            (str (first exp) ":")
          (= size 2)
            (str (first exp) " " (pbnj.jess/compile (second exp)) ":")
          :else
            (throw "a colon statement should be a list of 1 or 2 elements"))))

(define-function emit-control-flow [exp]
  (let [size (count exp)]
    (cond (>= size 3)
            (str (first exp) "(" (pbnj.jess/compile (second exp)) "){" (emit-block (rest (rest exp))) "}")
          :else
            (throw "a control flow statement should be a list of 3 elements"))))

(define-function emit-named-block [exp]
  (let [size (count exp)]
    (cond (>= size 2)
            (str (first exp) "{" (emit-block (rest exp)) "}")
          :else
            (throw "a named block should be a list of at least 2 elements"))))

(define-function emit-for-loop [exp]
  (let [size (count exp)
        terms (second exp)]
    (cond (>= size 3)
            (str "for(" (join (map terms compile) ";")  "){" (emit-block (rest (rest exp))) "}")
          :else
            (throw "a for loop should be a list of at least 3 elements"))))

(define-function emit-object-resolution [exp]
  (let [size (count exp)
        obj (second exp)
        prop (second (rest exp))]
    (cond (= size 3)
            (str (pbnj.jess/compile obj) "[" (if (number? prop) prop (str "'" prop "'")) "]")
          :else
            (throw "property access should be a list of 3 elements"))))

(define-function emit-method-call [exp]
  (let [size (count exp)
        obj (second exp)
        method (second (rest exp))
        args (rest (rest (rest exp)))]
    (cond (>= size 3)
            (pbnj.jess/compile (cons (list '.- obj (str method)) args))
          :else
            (throw "a method call should be a list of at least 3 elements"))))

(define-function pbnj.jess/emit-class-init [exp]
  (let [size (count exp)]
    (cond (= size 2)
            (str "(new " (pbnj.jess/compile (second exp)) "())")
          (> size 2)
            (str "(new " (pbnj.jess/compile (second exp)) "(" (join (map (rest (rest exp)) pbnj.jess/compile) ",") "))"))))

(define-function emit-property-assignment [exp]
  (let [size (count exp)
        obj (second exp)
        prop (second (rest exp))
        value (second (rest (rest exp)))]
    (cond (= size 4)
            (if (vector? prop)
              (str (pbnj.jess/compile obj) "[" (join (map prop pnbj.jess/compile) "][") "]=" (pbnj.jess/compile value))
              (str (pbnj.jess/compile obj) "[" (pbnj.jess/compile prop) "]=" (pbnj.jess/compile value)))
          :else
            (throw "property assignment should be a list of 4 elements"))))

(define-function pbnj.jess/emit-assignment [exp]
  (let [size (count exp)
        obj (second exp)
        value (second (rest exp))]
    (cond (= size 3)
            (str "(" (pbnj.jess/compile obj) "="
                 (if (symbol? value) value (pbnj.jess/compile value)) ")")
          :else
            (throw "assignment should be a list of 3 elements"))))

(define-function emit-unary-operator [exp]
  (let [size (count exp)]
    (cond (= size 2)
            (str (first exp) (second exp))
          :else
            (throw "a unary operator should be a list of 2 elements"))))

(define-function emit-binary-operator [exp]
  (let [size (count exp)]
    (cond (>= size 3)
            (join (map (rest exp) pbnj.jess/compile) (str (first exp)))
          :else
            (throw "a binary operator should be a list of at least 3 elements"))))

(define-function emit-negation [exp]
  (let [size (count exp)]
    (cond (= size 2)
            (emit-unary-operator (list '! (second (pbnj.jess/compile exp))))
          :else
            (throw "negation should be a list of 2 elements"))))

(define-function emit-quote [exp] (JSON/stringify exp))

(define-function emit-function-application [exp]
  (let [size (count exp)]
    (cond (= size 1)
            (str (pbnj.jess/compile (first exp)) "()")
          (> size 1)
            (str (pbnj.jess/compile (first exp)) "(" (join (map (rest exp) pbnj.jess/compile) ",") ")")
          :else
            (throw "function application should be a list of at least 1 element"))))

(define-function pbnj.jess/emit-object [exp]
  (str "({"
       (reduce exp
               (lambda [s pair]
                       (str (if (nil? s) "" (str s ","))
                            (pbnj.jess/compile (first pair))
                            ":"
                            (pbnj.jess/compile (second pair)))) nil) "})"))

(define-function pbnj.jess/emit-array [exp]
  (str "([" (join (map exp pbnj.jess/compile) ",") "])"))

(define MACROS {})

(define-function eval-macro-definition [exp]
  (let [size (count exp)]
    (cond (= size 4)
            (let [name (first (rest exp))
                  args (first (rest (rest exp)))
                  body (rest (rest (rest exp)))
                  fn (pbnj.wonderscript/eval (cons 'lambda (cons args body)))]
              (set! MACROS (assoc MACROS name fn))
              nil) ) ))

(define-function pbnj.jess/macroexpand [exp]
  (let [tag (first exp)
        xfr (get MACROS tag)]
    (if xfr (apply xfr (rest exp)) exp)))

(define-function pbnj.jess/compile [exp_]
  (let [exp (macroexpand exp_)]
    (cond (nil? exp) (emit-nil exp)
          (number? exp) (emit-number exp)
          (boolean? exp) (emit-boolean exp)
          (symbol? exp) (emit-symbol exp)
          (keyword? exp) (emit-symbol exp)
          (string? exp) (emit-string exp)
          (map? exp) (emit-object exp)
          (vector? exp) (emit-array exp)
          (list? exp)
            (let [tag (first exp)]
              (cond (= tag 'if-else) (emit-if-else exp)
                    (= tag 'if) (emit-if exp)
                    (= tag '?) (emit-existential exp)
                    (= tag 'instance?) (emit-instanceof exp)
                    (= tag 'type) (emit-typeof exp)
                    (= tag 'label) (emit-label exp)
                    (= tag 'do) (emit-block (rest exp))
                    (pbnj.jess/definition? exp) (emit-definition exp)
                    (or (= tag 'function) (= tag 'function*))
                      (emit-function exp)
                    (= tag 'return)
                      (apply emit-return (rest exp))
                    (has? '#{break continue throw delete} tag)
                      (emit-statement exp)
                    (or (= tag 'case) (= tag 'default))
                      (emit-colon-statment exp)
                    (has? '#{catch while switch} tag)
                      (emit-control-flow exp)
                    (= tag 'for) (emit-for-loop exp)
                    (= tag 'try) (emit-named-block exp)
                    (= tag '.-) (emit-object-resolution exp)
                    (= tag '.) (emit-method-call exp)
                    (= tag 'new) (emit-class-init exp)
                    (= tag '.-set!) (emit-property-assignment exp)
                    (= tag 'set!) (emit-assignment exp)
                    (or (= tag '!) (= tag 'not)) (emit-negation exp)
                    (has? '#{++ -- ~} tag) (emit-unary-operator exp)
                    (has? '#{|| && | & << >> % < > <= >= + - / * == != === !==} tag)
                      (emit-binary-operator exp)
                    (= tag 'quote) (emit-quote exp)
                    (= tag 'macro) (eval-macro-definition exp)
                    (= tag 'paren) (emit-expression exp)
                    (= tag 'comma) ","
                    (= tag 'semi) ";"
                    (= tag 'comment) ""
                    :else 
                      ; method resolution and class instantiation short require regex or JS string functions
                      (emit-function-application exp) )
          :else
            (do
              (throw (str "invalid form: '" exp "'"))) )))
  )

(define-function pbnj.jess/eval [exp]
  (let [code (pbnj.jess/compile exp)]
    (println (inspect code))
  (js/eval code)))

(define-function compile-stream [stream]
  (list 'function []
        (list 'var 'buffer [])
        (list 'while (list '! (list '. stream 'eof))
              (list '. 'buffer 'push (list 'eval (list 'pbnj.jess.compile (list '. 'stream 'next)))))
        (list '. 'buffer 'join "\n")))

(define-function pbnj.jess/compile-string [input source]
  (pbnj.jess/compile-stream (pbnj.reader/readString input source)))

(println (str *source* ":" *line*))
(pbnj.jess/readFile "src/pbnj/jess/core.jess")
