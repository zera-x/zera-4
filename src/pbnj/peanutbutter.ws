; vim: ft=clojure
(require "jelly.ws")
(require "jess.ws")
(require "phay.ws")
(require "wonderscript/compiler.ws")
(module pbnj.peanutbutter)

(define render-declaration pbnj.jelly/render-declaration)
(define join pbnj.core/join)

(define *components* (atom {}))

(define-function nil? [exp]
  (or (pbnj.core/nil? exp) (and (collection? exp) (empty? exp))))

(define render-nil (always ""))

(define-function tag? [exp]
  (and (sequential? exp)
       (let [nm (first exp)]
         (or (symbol? nm) (keyword? nm)))))

(define-function- render-tag-name [tname]
  (let [ns (namespace tname)
        nm (name tname)]
    (if ns
      (str ns ":" nm)
      nm)))

(define-function- render-single-tag [tag]
  (str "<" (render-tag-name (first tag)) " />"))

(define EVENTS #{:onclick :onblur :onfocus})

(define-function- render-content-tag [tag]
  (let [nm (render-tag-name (first tag))]
    (str "<" nm ">" (render-expression-list (rest tag)) "</" nm ">")))

(define-function- render-attr [pair]
  (let [attr (pair 0)
        kattr (if (symbol? attr) (keyword (name attr)) (keyword attr))
        v (pair 1)
        value (cond (and (= kattr :style) (map? v)) (render-declaration v)
                    (EVENTS kattr) (. (pbnj.wonderscript/compile v) (replace (new js/RegExp "\"" "g") "\\\""))
                    :else
                      (str v))]
    (str (name attr) "=\"" value "\"")))

(define-function render-attrs [attrs]
  (reduce (mori/map render-attr attrs) (lambda [s x] (str s " " x))))

(define-function- render-attrs-tag [tag]
  (let [nm (render-tag-name (first tag))
        attrs (render-attrs (second tag))]
    (str "<" nm " " attrs ">" (render-expression-list (rest (rest tag))) "</" nm ">")))

(define-function render-tag [tag]
  (cond (attrs? (second tag)) (render-attrs-tag tag)
        (> (count tag) 1) (render-content-tag tag)
        :else
          (render-single-tag tag)))

(define attrs? map?)

(define expression-list? sequential?)

(define-function render-expression-list [exprs]
  (reduce (mori/map html exprs) str))

(define-function component-index [nm]
  (. (str nm) (replace (new js/RegExp "^:") "")))

(define-function define-component [nm fn]
  (swap! *components* (lambda [comps] (assoc comps (component-index nm) fn))) nil)

(define-macro component
  ([nm x]
   (list 'pbnj.peanutbutter/define-component nm (list 'lambda [] x)))
  ([nm args &body]
   (list 'pbnj.peanutbutter/define-component nm (list 'lambda (cons args body)))))

(define-function components [] *components*)

(define-function definition? [exp]
  (and (list? exp) (= (first exp) 'define)))

(define-function eval-definition
  ([nm value]
   (define-component nm (always value)))
  ([nm args &body]
   (unless (vector? args) (throw "argument list should be a vector"))
   (define-component nm (eval (cons 'lambda (cons args body))))))

(define-function have-component? [nm]
  (has-key? (deref *components*) (component-index nm)))

(define-function get-component [nm]
  (get (deref *components*) (component-index nm)))

(define-function component? [exp]
  (and (tag? exp) (have-component? (first exp))))

(define-function render-component [exp]
  (let [component (get-component (first exp))]
    (html (apply component (rest exp)))))

(define-function atom? [exp]
  (or (number? exp) (boolean? exp) (string? exp) (symbol? exp)))

(define render-atom str)

(define-function html-encode [s]
  (reduce (map (into [] (. s (split ""))) (lambda [c] (str "&#" (. c charCodeAt) ";"))) str))

(define-function expression-escape? [exp]
  (and (list? exp) (= (first exp) '=)))

(define-function block? [exp]
  (and (list? exp) (= (first exp) 'do)))

(define *top-scope* (pbnj/env))
(define-function eval-block [&body]
  (let [ret nil]
    (do-each [x body]
      (set! ret (eval x *top-scope*)))
    ret))

(define-function html
  [exp]
  (cond (nil? exp) (render-nil)
        (atom? exp) (render-atom exp)
        (block? exp) (apply eval-block (rest exp))
        (definition? exp) (apply eval-definition (rest exp))
        (component? exp) (render-component exp)
        (tag? exp) (render-tag exp)
        (expression-list? exp) (render-expression-list exp)
        :else
          (do
            (println exp)
            (throw "invalid expression"))))

(define compile html)

(define-function render-to [elem expr])

(define-function render [expr]
  (. js/document (write (html expr))))

(define-function compile-stream
  [stream]
  (let [buffer (array)]
    (until (. stream eof)
      (. buffer (push (compile (. stream next)))))
    (. buffer (join \newline))))

(define-function compile-string
  [input source]
  (compile-stream (pbnj.reader/readString input source)))

(define-function compile-file
  [file]
  (compile-stream (pbnj.reader/readFile file)))

(define-component :javascript
  (lambda
    [code]
    [:script {:type "text/javascript"}
     (if (string? code)
       code
       (pbnj.jess/compile code))]))

(define-component :wonderscript
  (lambda
    [code]
    [:script {:type "text/javascript"} (pbnj.wonderscript/compile code)]))

(define-component :php
  (lambda
    [code]
    (pbnj.phay/compile (list '<?php code))))

(test html
  (is (= "<br />" (html [:br])))
  (is (= "<br />" (html '(br))))
  (is (= "<b>Test</b>" (html [:b "Test"])))
  (is (= "<a href=\"#\">Test</a>" (html [:a {:href "#"} "Test"])))
  (is (= "<script type=\"text/javascript\">alert(\"test\")</script>"
         (html [:javascript "alert(\"test\")"])))
  (is (= "<script type=\"text/javascript\">alert(\"test\")</script>"
         (html [:javascript '(alert "test")])))
  (is (= "<script type=\"text/javascript\">alert(\"test\")</script>"
         (html [:wonderscript '(alert "test")]))))
