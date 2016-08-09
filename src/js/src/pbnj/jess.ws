; vim: ft=clojure
(module 'pbnj.jess)

(define-function emit-nil [exp] "null")
(define-function emit-number [exp] (str exp))
(define-function emit-boolean [exp] (if exp "true" "false"))

(define-function emit-symbol [exp]
  (let [nm (name exp)
        ns (namespace exp)]
    (if (nil? ns) nm (str ns "." name))))

(define-function emit-string [exp]
  (str "\"" exp "\""))

(define-function emit-expression [exp]
  (str "(" compile(exp) ")"))

(define-function emit-block [exp]
  (str "{" (join (map exp compile) ";") "}"))

(define-function emit-if-else [exp]
  (let [size (count exp)]
    (unless (and (list? exp) (<= 3 size) (odd? size))
            (throw "malformed expression: an if-else should be a list with at least 3 elements"))
    (reduce (pair (rest exp))
      (lambda [memo exp]
              (cond (nil? memo)
                      (str "if(" (compile (exp 0)) "){" (compile (exp 1)) "}")
                    (= 'else (exp 0))
                      (str memo "else{" (compile (exp 1)) "}")
                    :else
                      (str memo "else if(" (compile (exp 0)) "){" (compile (exp 1)) "}"))) nil)))

(define-function emit-if [exp]
  (let [pred (second exp)
        conse (first (rest (rest exp)))
        alt (second (rest (rest exp)))]
    (str "(" (compile pred) ")?(" (compile conse) "):(" (compile alt) ")")))

(define-function emit-existential [exp]
  (let [value (compile (second exp))]
    (str "((typeof " value "!==\"undefined\")&&(" value "!==null))")))

(define-function emit-instanceof [exp]
  (str "(" (compile (second exp)) " instanceof " (compile (first (rest (rest exp)))) ")"))

(define-function emit-typeof [exp]
  (str "(typeof " (compile (second exp)) ")"))

(define-function emit-label [exp]
  (str (name (second exp)) ":"))

(define-function emit-definition [exp]
  (let [size (count exp)]
    (cond (= size 2)
            (str (first exp) " " (second exp) ";")
          (= size 3)
            (str (first exp) " " (second exp) "=" (compile (first (rest (rest exp)))) ";")
          :else
            (throw "malformed expression: a definition should be a list of 2 or 3 elements"))))

(define-function emit-argument-list [args]
 (str "(" (join (map args compile) ",") ")"))

(define-function emit-function [exp]
  (let [size (count exp)]
    (cond (= size 3)
            (str "(" (first exp) (emit-argument-list (second exp)) (emit-block (rest (rest exp))) ")")
          (= size 4)
            (str (first exp) " " (second exp) (emit-argument-list (second (rest exp))) (emit-block (rest (rest (rest exp)))))
          :else
            (throw "a function should be a list of 3 or 4 elements"))))

(define-function emit-statement [exp]
  (let [size (count exp)]
    (cond (= size 1)
            (str (first exp) ";")
          (= size 2)
            (str (first exp) " " (compile (second exp)) ";")
          :else
            (throw "a statement should be a list of 1 or 2 elements"))))

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
    (cond (= size 3)
            (str (first exp) "(" (compile (second exp)) ")" (emit-block (rest (rest exp))))
          :else
            (throw "a control flow statement should be a list of 3 elements"))))

(define-function emit-named-block [exp]
  (let [size (count exp)]
    (cond (= size 2)
            (str (first exp) (emit-block (rest exp)))
          :else
            (throw "a named block should be a list of 2 elements"))))

(define-function emit-for-loop [exp]
  (let [size (count exp)
        terms (second exp)]
    (cond (>= size 3)
            (str "for(" (join (map terms compile) ";")  ")" (emit-block (rest (rest exp))))
          :else
            (throw "a for loop should be a list of at least 3 elements"))))

(define-function emit-object-resolution [exp]
  (let [size (count exp)
        obj (second exp)
        prop (second (rest exp))]
    (cond (= size 3)
            (str (compile obj) "['" prop "']")
          :else
            (throw "property access should be a list of 3 elements"))))

(define-function emit-method-call [exp]
  (let [size (count exp)
        obj (second exp)
        method (second (rest exp))
        args (rest (rest (rest exp)))]
    (cond (>= size 3)
            (str (compile obj) "['" method "'](" (join (map args compile) ",") ")")
          :else
            (throw "a method call should be a list of at least 3 elements"))))

(define-function emit-class-init [exp]
  (let [size (count exp)
        class (second exp)
        args (rest (rest exp))]
    (cond (>= size 2)
            (str "(new" (compile class) "(" (join (map args compile) ",") "))")
          :else
            (throw "class instantiation should be a list of at least 2 elements"))))

(define-function emit-property-assignment [exp]
  (let [size (count exp)
        obj (second exp)
        prop (second (rest exp))
        value (second (rest (rest exp)))]
    (cond (= size 4)
            (str "(" (compile obj) "['" prop "']=" (compile value) ")")
          :else
            (throw "property assignment should be a list of 4 elements"))))

(define-function emit-assignment [exp]
  (let [size (count exp)
        obj (second exp)
        value (second (rest (rest exp)))]
    (cond (= size 3)
            (str "(" (compile obj) "=" (compile value) ")")
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
            (join (map (rest exp) compile) (str (first exp)))
          :else
            (throw "a binary operator should be a list of at least 3 elements"))))

(define-function emit-negation [exp]
  (let [size (count exp)]
    (cond (= size 2)
            (emit-unary-operator (list '! (second exp)))
          :else
            (throw "negation should be a list of 2 elements"))))

(define-function emit-quote [exp] (JSON/stringify exp))

(define-function emit-function-application [exp]
  (let [size (count exp)]
    (cond (= size 1)
            (str (compile (first exp)) "()")
          (> size 1)
            (str (compile (first exp)) "(" (join (map (rest exp) compile) ",") ")")
          :else
            (throw "function application should be a list of at least 1 element"))))

(define-function emit-object [exp]
  (str "{"
       (reduce exp
               (lambda [s value key x]
                       (str s "," key ":" (compile value)))) "}"))

(define-function pbnj.jess/compile [exp]
  (do (println "exp: " exp)
  (cond (nil? exp) (emit-nil exp)
        (number? exp) (emit-number exp)
        (boolean? exp) (emit-boolean exp)
        (symbol? exp) (do (println "symbol" exp) (emit-symbol exp))
        (keyword? exp) (emit-symbol exp)
        (string? exp) (emit-string exp)
        (list? exp)
          (let [tag (first exp)]
            (cond (= tag 'if-else) (emit-if-else exp)
                  (= tag 'if) (emit-if exp)
                  (= tag '?) (emit-existential exp)
                  (= tag 'instance?) (emit-instanceof exp)
                  (= tag 'type) (emit-typeof exp)
                  (= tag 'label) (emit-label exp)
                  (= tag 'do) (emit-block (rest exp))
                  (has? '#{var let const} tag)
                    (emit-definition exp)
                  (or (= tag 'function) (= tag 'function*))
                    (emit-function exp)
                  (has? '#{return break continue throw delete} tag)
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
                  :else 
                    ; method resolution and class instantiation short require regex or JS string functions
                    (emit-function-application exp) ))
        (object? exp)
          (emit-object exp)
        :else
          (throw (str "invalid form: '" exp "'")) ))
  )

(define-function pbnj.jess/eval [exp]
  (js/eval (compile exp)))
