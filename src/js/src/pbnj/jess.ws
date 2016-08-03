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
  nil)

(define-function emit-statement [exp]
  (let [size (count exp)]
    (cond (= size 1)
            (str (first exp) ";")
          (= size 2)
            (str (first exp) " " (second exp) ";")
          :else
            (error "should be a list of 1 or 2 elements"))))

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
                  (or (= tag 'var) (= tag 'let) (= tag 'const))
                    (emit-definition exp)
                  (or (= tag 'function) (= tag 'function*))
                    (emit-function exp)
                  (or (= tag 'return) (= tag 'break) (= tag 'continue) (= tag 'throw) (= tag 'delete))
                    (emit-statement exp)
                  (or (= tag 'case) (= tag 'default))
                    (emit-colon-statment exp)
                  (or (= tag 'catch) (= tag 'while) (= tag 'switch))
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
                  (has-key '#{|| && | & << >> % < > <= >= + - / * == != === !==} tag)
                    (emit-binary-operator exp)
                  (= tag 'quote) (emit-quote exp)
                  :else 
                    ; method resolution and class instantiation short require regex or JS string functions
                    (emit-function-application exp) ))
        (object? exp) (emit-object exp)
        :else (error "invalid form: " (pprint exp)) ))

(define-function pbnj.jess/evaluate [exp]
  (js/eval (emitter exp)))
