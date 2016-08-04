; vim: ft=clojure
(module 'pbnj.jess)

(define-function error [msg]
  (throw (init js/Error msg)))

(define-function emit-nil [exp] "null")
(define-function emit-number [exp] (str exp))
(define-function emit-boolean [exp] (if true "true" "false"))

(define-function emit-symbol [exp]
  (let [nm (name exp)
        ns (namespace exp)]
    (if (nil? ns) nm (str ns "." name))))

(define-function emit-string [exp]
  (str "\"" exp "\""))

(define-function emit-expression [exp]
  (str "(" emitter(exp) ")"))

(define-function emit-block [exp]
  (str "{" (join (map exp emitter) ";") "}"))

(define-function emit-if-else [exp]
  (let [size (count exp)]
    (unless (and (list? exp) (<= 3 size) (odd? size))
            (error "malformed expression: an if-else should be a list with at least 3 elements"))
    (reduce (pair (rest exp))
      (lambda [memo exp]
              (cond (nil? memo)
                      (str "if(" (emitter (exp 0)) "){" (emitter (exp 1)) "}")
                    (= 'else (exp 0))
                      (str memo "else{" (emitter (exp 1)) "}")
                    :else
                      (str memo "else if(" (emitter (exp 0)) "){" (emitter (exp 1)) "}"))) nil)))

(define-function emit-if [exp]
  (let [pred (second exp)
        conse (first (rest (rest exp)))
        alt (second (rest (rest exp)))]
    (str "(" (emitter pred) ")?(" (emitter conse) "):(" (emitter alt) ")")))

(define-function emit-existential [exp]
  (let [value (emitter (second exp))]
    (str "((typeof " value "!==\"undefined\")&&(" value "!==null))")))

(define-function emit-instanceof [exp]
  (str "(" (emitter (second exp)) " instanceof " (emitter (first (rest (rest exp)))) ")"))

(define-function emit-typeof [exp]
  (str "(typeof " (emitter (second exp)) ")"))

(define-function emit-label [exp]
  (str (name (second exp)) ":"))

(define-function emit-definition [exp]
  (let [size (count exp)]
    (cond (= size 2)
            (str (first exp) " " (second exp) ";")
          (= size 3)
            (str (first exp) " " (second exp) "=" (emitter (first (rest (rest exp)))) ";")
          :else
            (error "malformed expression: a definition should be a list of 2 or 3 elements"))))

(define-function emit-function [exp]
  (let [size (count exp)]
    (cond (= size 3)
            (str (first exp) "(" (emitter (second exp)) ")" (emit-block (rest (rest exp))))
          (= size 4)
            (str (first exp) " " (second exp) "(" (emitter (second (rest exp))) ")" (emit-block (rest (rest (rest exp)))))
          :else
            (error "should be a list of 3 or 4 elements"))))

(define-function emit-statement [exp]
  (let [size (count exp)]
    (cond (= size 1)
            (str (first exp) ";")
          (= size 2)
            (str (first exp) " " (emitter (second exp)) ";")
          :else
            (error "should be a list of 1 or 2 elements"))))

(define-function emit-colon-statement [exp]
  (let [size (count exp)]
    (cond (= size 1)
            (str (first exp) ":")
          (= size 2)
            (str (first exp) " " (emitter (second exp)) ":")
          :else
            (error "should be a list of 1 or 2 elements"))))

(define-function emit-control-flow [exp]
  (let [size (count exp)]
    (cond (= size 3)
            (str (first exp) "(" (emitter (second exp)) ")" (emit-block (rest (rest exp))))
          :else
            (error "should be a list of 3 elements"))))

(define-function emit-named-block [exp]
  (let [size (count exp)]
    (cond (= size 2)
            (str (first exp) (emit-block (rest exp)))
          :else
            (error "should be a list of 2 elements"))))

(define-function emit-for-loop [exp]
  (let [size (count exp)
        terms (second exp)]
    (cond (= size 3)
            (str "for(" (join (map terms emitter) ";")  ")" (emit-block (rest (rest exp))))
          :else
            (error "should be a list of 3 elements"))))

(define-function emit-object-resolution [exp]
  (let [size (count exp)
        obj (second exp)
        prop (second (rest exp))]
    (cond (= size 3)
            (str (emitter obj) "['" prop "']")
          :else
            (error "should be a list of 3 elements"))))

(define-function emit-method-call [exp]
  (let [size (count exp)
        obj (second exp)
        method (second (rest exp))
        args (rest (rest (rest exp)))]
    (cond (>= size 3)
            (str (emitter obj) "['" method "'](" (join (map args emitter) ",") ")")
          :else
            (error "should be a list of at least 3 elements"))))

(define-function emit-class-init [exp]
  (let [size (count exp)
        class (second exp)
        args (rest (rest exp))]
    (cond (>= size 2)
            (str "(new" (emitter class) "(" (join (map args emitter) ",") "))")
          :else
            (error "should be a list of at least 2 elements"))))

(define-function emit-property-assignment [exp]
  (let [size (count exp)
        obj (second exp)
        prop (second (rest exp))
        value (second (rest (rest exp)))]
    (cond (= size 4)
            (str "(" (emitter obj) "['" prop "']=" (emitter value) ")")
          :else
            (error "should be a list of 4 elements"))))

(define-function emit-assignment [exp]
  (let [size (count exp)
        obj (second exp)
        value (second (rest (rest exp)))]
    (cond (= size 3)
            (str "(" (emitter obj) "=" (emitter value) ")")
          :else
            (error "should be a list of 3 elements"))))

(define-function emit-unary-operator [exp]
  (let [size (count exp)]
    (cond (= size 2)
            (str (first exp) (second exp))
          :else
            (error "should be a list of 2 elements"))))

(define-function emit-binary-operator [exp]
  (let [size (count exp)]
    (cond (>= size 3)
            (join (map (rest exp) emitter) (str (first exp)))
          :else
            (error "should be a list of at least 3 elements"))))

(define-function emit-negation [exp]
  (let [size (count exp)]
    (cond (= size 2)
            (emit-unary-operator (list '! (second exp)))
          :else
            (error "should be a list of 2 elements"))))

(define-function emit-quote [exp] (JSON/stringify exp))

(define-function pbnj.jess/emitter [exp]
  (cond (nil? exp) (emit-nil exp)
        (number? exp) (emit-number exp)
        (boolean? exp) (emit-boolean exp)
        (symbol? exp) (emit-symbol exp)
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
                  (has '#{var let const} tag)
                    (emit-definition exp)
                  (or (= tag 'function) (= tag 'function*))
                    (emit-function exp)
                  (has '#{return break continue throw delete} tag)
                    (emit-statement exp)
                  (or (= tag 'case) (= tag 'default))
                    (emit-colon-statment exp)
                  (has '#{catch while switch} tag)
                    (emit-control-flow exp)
                  (= tag 'for) (emit-for-loop)
                  (= tag 'try) (emit-named-block exp)
                  (= tag '.-) (emit-object-resolution exp)
                  (= tag '.) (emit-method-call exp)
                  (= tag 'new) (emit-class-init exp)
                  (= tag '.-set!) (emit-property-assignment exp)
                  (= tag 'set!) (emit-assignment exp)
                  (or (= tag '!) (= tag 'not)) (emit-negation exp)
                  (or (= tag '++) (= tag '--) (= tag '~)) (emit-unary-operator exp)
                  (has '#{|| && | & << >> % < > <= >= + - / * == != === !==} tag)
                    (emit-binary-operator exp)
                  (= tag 'quote) (emit-quote exp)
                  :else 
                    ; method resolution and class instantiation short require regex or JS string functions
                    (emit-function-application exp) ))
        (object? exp) (emit-object exp)
        :else (error "invalid form: " (pprint exp)) ))

(define-function pbnj.jess/evaluate [exp]
  (js/eval (emitter exp)))
