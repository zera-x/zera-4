; vim: ft=clojure
(ns pbnj.phay)

; compile PHP code
(define join pbnj.core/join)

(define-function emit-nil [exp] "null")
(define-function emit-number [exp] (str exp))
(define-function emit-boolean [exp] (if exp "true" "false"))

(define-function emit-symbol [exp]
  (let [nm (name exp)
        ns (namespace exp)]
    (if (nil? ns) nm (str "\\" (.. ns (split ".") (join "\\")) "\\" nm))))

(define-function emit-string [exp] (str "'" exp "'"))

(define-function emit-expression [exp] (str "(" (compile (second exp)) ")"))

(define-function emit-block [exp] (str (join (map compile exp) ";") ";"))

(define-function emit-if-else [exp]
  (let [size (count exp)]
    (unless (and (list? exp) (<= 3 size) (odd? size))
            (throw "malformed expression: an if-else should be a list with at least 3 elements"))
    (reduce
      (lambda [memo exp]
              (cond (nil? memo)
                      (str "if(" (compile (exp 0)) "){" (compile (exp 1)) "}")
                    (= :else (exp 0))
                      (str memo "else{" (compile (exp 1)) "}")
                    :else
                      (str memo "elseif(" (compile (exp 0)) "){" (compile (exp 1)) "}")))
      nil
      (pair (rest exp)))))

(define-function emit-if [exp]
  (let [pred (second exp)
        conse (first (rest (rest exp)))
        alt (second (rest (rest exp)))]
    (str "(" (compile pred) ")?(" (compile conse) "):(" (compile alt) ")")))

(define-function emit-existential [exp]
  (let [value (compile (second exp))]
    (str "isset(" value ")")))

(define-function emit-escape [exp]
  (str "<?php " (str (join (map compile (rest exp)) ";") ";") " ?>"))

(define-function emit-cast [t exp]
  (str "(" t ")" (compile exp)))

(define-function definition? [exp]
  (let [tag (first exp)]
    (or (= tag 'var) (= tag 'public) (= tag 'const) (= tag 'private) (= tag 'protected))))

(define-function emit-definition [exp]
  (let [size (count exp)]
    (cond (= size 2)
            (str (first exp) " " (second exp) ";")
          (= size 3)
            (str (first exp) " " (second exp) "=" (compile (first (rest (rest exp)))) ";")
          :else
            (throw "malformed expression: a definition should be a list of 2 or 3 elements"))))

(define-function emit-argument-list [args]
  (if (empty? args) "()"
      (str "(" (join (map compile args) ",") ")")))

(define-function emit-function [exp]
  (let [ident (second exp)]
    (cond (sequential? ident)
            (str (first exp) (emit-argument-list (second exp)) "{" (emit-block (rest (rest exp))) "}")
          (symbol? ident)
            (str (first exp) " " (second exp) (emit-argument-list (second (rest exp))) "{" (emit-block (rest (rest (rest exp)))) "}")
          :else
            (throw "malformed function expression"))))

(define-function emit-peren [exp]
  (str "(" (compile exp) ")"))

(define-function emit-statement [exp]
  (let [size (count exp)]
    (cond (= size 1)
            (str (first exp) ";")
          (= size 2)
            (str (first exp) " " (compile (second exp)) ";")
          :else
            (throw "a statement should be a list of 1 or 2 elements"))))

(define-function emit-return 
  ([] "return;")
  ([val]
   (if (definition? val)
     (str (emit-definition val) "return " (second val) ";")
     (str "return " (compile val) ";"))))

(define-function emit-colon-statement [exp]
  (let [size (count exp)]
    (cond (= size 1)
            (str (first exp) ":")
          (= size 2)
            (str (first exp) " " (compile (second exp)) ":")
          :else
            (throw "a colon statement should be a list of 1 or 2 elements"))))

(define-function emit-control-flow [exp]
  (let [size (count exp)]
    (cond (>= size 3)
            (str (first exp) "(" (compile (second exp)) "){" (emit-block (rest (rest exp))) "}")
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
            (str "for(" (join (map compile terms) ";")  "){" (emit-block (rest (rest exp))) "}")
          :else
            (throw "a for loop should be a list of at least 3 elements"))))

(define-function emit-object-resolution [exp]
  (let [size (count exp)
        obj (second exp)
        prop (second (rest exp))]
    (cond (= size 3)
            (if (vector? prop)
              (str (compile obj) "['" (join prop "']['") "']")
              (str (compile obj) "[" (if (number? prop) prop (str "'" prop "'")) "]"))
          :else
            (throw "property access should be a list of 3 elements"))))

(define-function emit-method-call [exp]
  (let [size (count exp)
        obj (second exp)
        method (second (rest exp))
        args (rest (rest (rest exp)))]
    (cond (>= size 3)
            (compile (cons (list '.- obj (str method)) args))
          :else
            (throw "a method call should be a list of at least 3 elements"))))

(define-function emit-class-init [exp]
  (let [size (count exp)]
    (cond (= size 2)
            (str "new " (compile (second exp)) "()")
          (> size 2)
            (str "new " (compile (second exp)) "(" (join (map compile (rest (rest exp))) ",") ")"))))

(define-function emit-property-assignment [exp]
  (let [size (count exp)
        obj (second exp)
        prop (second (rest exp))
        value (second (rest (rest exp)))]
    (cond (= size 4)
            (if (vector? prop)
              (str (compile obj) "['" (join prop "']['") "']=" (compile value))
              (str (compile obj) "['" prop "']=" (compile value)))
          :else
            (throw "property assignment should be a list of 4 elements"))))

(define-function emit-assignment [obj value]
  (str (compile obj) "="
       (if (symbol? value) value (compile value))))

(define-function emit-unary-operator [exp]
  (let [size (count exp)]
    (cond (= size 2)
            (str (first exp) (second exp))
          :else
            (throw "a unary operator should be a list of 2 elements"))))

(define-function emit-binary-operator [exp]
  (let [size (count exp)]
    (cond (>= size 3)
            (join (map compile (rest exp)) (str (first exp)))
          :else
            (throw "a binary operator should be a list of at least 3 elements"))))

(define-function emit-negation [exp]
  (let [size (count exp)]
    (cond (= size 2)
            (emit-unary-operator (list '! (second (compile exp))))
          :else
            (throw "negation should be a list of 2 elements"))))

(define-function emit-quote [exp] (JSON/stringify exp))

(define-function emit-function-application
  ([fn]
   (str (compile fn) "()"))
  ([fn &args]
   (str (compile fn) "(" (join (map compile args) ",") ")")))

(define-function emit-assoc-array [exp]
  (str "["
       (reduce
        (lambda [s pair]
                (str (if (nil? s) "" (str s ","))
                    (compile (first pair))
                    "=>"
                    (compile (second pair)))) nil)
       "]"
       exp))

(define-function emit-array [exp]
  (if (empty? exp)
    "[]"
    (str "[" (reduce (lambda [s x] (str s ", " x))) "]" (map compile exp))))

(define *macros* (atom {}))

(define-function eval-macro-definition [exp]
  (let [size (count exp)]
    (cond (= size 4)
            (let [name (first (rest exp))
                  args (first (rest (rest exp)))
                  body (rest (rest (rest exp)))
                  fn (pbnj.wonderscript/eval (cons 'lambda (cons args body)))]
              (swap! *macros* (lambda [ms] (assoc ms name fn)))
              nil) ) ))

(define-function macroexpand [exp]
  (let [tag (first exp)
        xfr (get (deref *macros*) tag)]
    (if xfr
      (macroexpand (apply xfr (rest exp)))
      exp)))

(define-function compile [exp_]
  (let [exp (macroexpand exp_)]
    (cond (or (nil? exp) (= exp 'null)) (emit-nil exp)
          (number? exp) (emit-number exp)
          (boolean? exp) (emit-boolean exp)
          (symbol? exp) (emit-symbol exp)
          (keyword? exp) (emit-symbol exp)
          (string? exp) (emit-string exp)
          (map? exp) (emit-assoc-array exp)
          (vector? exp) (emit-array exp)
          (list? exp)
            (let [tag (first exp)]
              (cond (or (= tag 'if-else) (= tag 'cond)) (emit-if-else exp)
                    (= tag 'if) (emit-if exp)
                    (= tag '?) (emit-existential exp)
                    (or (= tag '<?php) (= tag '<?)) (emit-escape exp)
                    (= tag 'cast) (apply emit-cast (rest exp))
                    (= tag 'do) (emit-block (rest exp))
                    (definition? exp) (emit-definition exp)
                    (= tag 'function) (emit-function exp)
                    (= tag 'return) (apply emit-return (rest exp))
                    (has? '#{break continue throw global echo} tag) (emit-statement exp)
                    (or (= tag 'case) (= tag 'default)) (emit-colon-statment exp)
                    (has? '#{catch while switch} tag) (emit-control-flow exp)
                    (= tag 'for) (emit-for-loop exp)
                    (= tag 'try) (emit-named-block exp)
                    (= tag '.-) (emit-object-resolution exp)
                    (= tag '.) (emit-method-call exp)
                    (= tag 'new) (emit-class-init exp)
                    (= tag '.-set!) (emit-property-assignment exp)
                    (= tag 'set!) (apply emit-assignment (rest exp))
                    (or (= tag '!) (= tag 'not)) (emit-negation exp)
                    (or (= tag '++) (= tag '--) (= tag '&)) (emit-unary-operator exp)
                    (has? '#{|| && | & << >> % < > <= >= + - / * == != === !== and or} tag) (emit-binary-operator exp)
                    (or (= tag '%) (= tag 'mod)) (emit-binary-operator (cons '% (rest exp)))
                    (= tag 'quote) (emit-quote exp)
                    (= tag 'macro) (eval-macro-definition exp)
                    (= tag 'paren) (emit-expression exp)
                    (= tag 'comma) ","
                    (= tag 'semi) ";"
                    (= tag 'comment) ""
                    :else 
                      (do
                        (apply emit-function-application exp))))
          :else
            (do
              (throw (str "invalid form: '" exp "'"))))))
