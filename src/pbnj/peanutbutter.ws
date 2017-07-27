; vim: ft=clojure
(require "jelly.ws")
(require "jess.ws")
(require "phay.ws")
(require "wonderscript/compiler.ws")
(ns pbnj.peanutbutter)

(define render-declaration pbnj.jelly/render-declaration)

(define-protocol Expression)

(define-protocol HTML
  (toHTML [self]))

(define-protocol HTMLNode HTML)

(define-type Nothing
  []
  Expression
  HTMLNode
  (toHTML [self] ""))

(define nothing (Nothing.))

(define-type Atom
  [value]
  Expression
  HTMLNode
  (toHTML [self] (str (.-value self))))

(define-protocol HTMLTag HTML)

(define-type SimpleTag
  [name]
  Expression
  HTMLTag
  (toHTML [self] (str "<" (render-tag-name (.-name self)) ">")))

(define-type ContentTag
  [name children]
  Expression
  HTMLTag
  (toHTML [self]
    (let [nm (render-tag-name (.-name self))]
      (str "<" nm ">"
           (.toHTML (.-children self))
           "</" nm ">"))))

(define-type AttributeContentTag
  [name attrs children]
  Expression
  HTMLTag
  (toHTML [self]
    (let [nm (render-tag-name (.-name self))
          sattrs (render-attrs (.-attrs self))]
      (str "<" nm " " sattrs ">"
           (.toHTML (.-children self))
           "</" nm ">"))))

(define-type AttributeTag
  [name attrs]
  Expression
  HTMLTag
  (toHTML [self]
    (str "<" (render-tag-name (.-name self)) " " (render-attrs (.-attrs self)) ">")))

(define-type ExpressionList
  [expressions]
  Expression
  HTML
  (toHTML [self]
    (->> (.-expressions self)
         (map (& (.toHTML %)))
         (reduce str))))

(define-function nil? [exp]
  (or (pbnj.core/nil? exp) (and (collection? exp) (empty? exp))))

(define render-nil (constantly ""))

(define-function tag? [exp]
  (and (list? exp) (symbol? (first exp))))

(define-function tag
  [exp]
  (case (count exp)
    1 (SimpleTag. (first exp))
    2 (if (attrs? (second exp))
        (AttributeTag. (first exp) (second exp))
        (ContentTag. (first exp) (ExpressionList. (map analyze (rest exp)))))
    :else
      (if (attrs? (second exp))
        (AttributeContentTag. (first exp) (second exp) (ExpressionList. (map analyze (rest (rest exp)))))
        (ContentTag. (first exp) (ExpressionList. (map analyze (rest exp)))))))

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
                    (EVENTS kattr) (.replace (pbnj.wonderscript/compile v) (js/RegExp. "\"" "g") "\\\"")
                    (= kattr :class)
                      (if (sequential? v) (join " " v) v)
                    :else
                      (str v))]
    (str (name attr) "=\"" value "\"")))

(define-function render-attrs [attrs]
  (reduce (lambda [s x] (str s " " x)) (map render-attr attrs)))

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
  (reduce str (map html exprs)))

(define-macro define-component
  [nm &forms]
  (let [sym (if (keyword? nm) (symbol (namespace nm) (name nm)) nm)]
    (list 'define {:peanutbutter/component true} sym (cons 'lambda forms))))

(define-function definition? [exp]
  (and (list? exp) (= (first exp) 'define)))

(define-function eval-definition
  ([nm value]
   (define-component nm [] value))
  ([nm args &body]
   (unless (vector? args) (throw "argument list should be a vector"))
   (define-component )))

(define-function get-component
  {:memoize true}
  [exp]
  (-> exp var .get))

(define-function component?
  [exp]
  (-> (first exp) var .getMeta :peanutbutter/component))

(define-function render-component
  [exp]
  (let [component (get-component (first exp))]
    (html (apply component (rest exp)))))

(define-function atom? [exp]
  (or (number? exp) (boolean? exp) (string? exp) (symbol? exp)))

(define render-atom str)

(define-function html-encode [s]
  (reduce str (map (lambda [c] (str "&#" (. c charCodeAt) ";")) (into [] (. s (split ""))))))

(define-function expression-escape? [exp]
  (and (list? exp) (= (first exp) '=)))

(define-function block? [exp]
  (and (list? exp) (= (first exp) 'do)))

(define *top-scope* (pbnj.wonderscript/env))
(define-function eval-block [&body]
  (do
    (define {:private true :dynamic true} ret)
    (do-each [x body]
      (set! ret (eval x *top-scope*)))
    ret))

(define-function analyze
  [exp]
  (cond (nil? exp) nothing
        (atom? exp) (Atom. exp)
        (block? exp) (apply eval-block (rest exp))
        ;(definition? exp) (apply eval-definition (rest exp))
        (tag? exp) (tag exp)
        (expression-list? exp) (ExpressionList. (map analyze exp))
        :else
          (do
            (throw (new js/Error (str "invalid expression: " exp))))))

(define-function html
  [exp]
  (.toHTML (analyze exp)))

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

(define-component javascript
  [code]
  [:script {:type "text/javascript"}
    (if (string? code)
      code
      (pbnj.jess/compile code))])

(define-component wonderscript
  [code]
  [:script {:type "text/javascript"} (pbnj.wonderscript/compile code)])

(define-component php
  [code]
  (pbnj.phay/compile (list '<?php code)))

(define-component php=
  [code]
  (pbnj.phay/compile (list '<?php (list 'echo code))))

(define-component css
  [&rules]
  [:stype {:type "text/css"} (pbnj.jelly/css rules))

;(test html
;  (is (= "<br />" (html [:br])))
;  (is (= "<br />" (html '(br))))
;  (is (= "<b>Test</b>" (html [:b "Test"])))
;  (is (= "<a href=\"#\">Test</a>" (html [:a {:href "#"} "Test"])))
;  (is (= "<script type=\"text/javascript\">alert(\"test\")</script>"
;         (html [:javascript "alert(\"test\")"])))
;  (is (= "<script type=\"text/javascript\">alert(\"test\")</script>"
;         (html [:javascript '(alert "test")])))
;  (is (= "<script type=\"text/javascript\">alert(\"test\")</script>"
;         (html [:wonderscript '(alert "test")]))))
