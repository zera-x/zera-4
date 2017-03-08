; vim: ft=clojure
(module pbnj.jelly)

(define-function nil? [exp]
  (or (pbnj.core/nil? exp)
      (and (collection? exp) (empty? exp))))

(define-function atom? [exp]
  (or (symbol? exp) (keyword? exp) (string? exp)))

(define-function render-atom [exp]
  (cond (string? exp) exp
        (or (symbol? exp) (keyword? exp)) (name exp)
        :else
          (throw "invalid atom")))

(define-function render-number [exp] (str exp "px"))

(define-function rule? [exp]
  (and (sequential? exp) (map? (last exp))))

(define-function render-selector [exp]
  (reduce exp (lambda [s x] (str s " " x))))

(define-function render-declaration [exp]
  (reduce (map exp (lambda [pair] (str (name (pair 0)) ": " (css (pair 1)))))
          (lambda [s x] (str s ";" x))))

(define declaration? map?)

(define-function render-rule [exp]
  (str (render-selector (reject exp map?)) " { "
       (render-declaration (first (filter exp map?))) " } "))

(define-function css [exp]
  (cond (nil? exp) ""
        (number? exp) (render-number exp)
        (atom? exp) (render-atom exp)
        (declaration? exp) (render-declaration exp)
        (rule? exp) (render-rule exp)
        :else
          (throw "invalid form")))
