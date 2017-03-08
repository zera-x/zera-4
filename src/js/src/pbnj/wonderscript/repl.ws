; vim: ft=clojure
(require "src/pbnj/peanutbutter.ws")

(module pbnj.wonderscript.repl)

(define- html pbnj.peanutbutter/html)
(define- define-component pbnj.peanutbutter/define-component)
(define- render pbnj.peanutbutter/render)
(define- js pbnj.jess/compile)

(define-component :layout
  (lambda
    [&body]
  [:html {:lang "en"}
   [:head
    [:title "WonderScript Shell"]
    [:meta {:charset "utf-8"}]
    [:meta {:http-equiv "X-UA-Compatible" :content "IE=edge"}]
    [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
    [:link {:href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
            :integrity "sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u"
            :crossorigin "anonymous"}]]
   [:body
    [:div {:class "container-fluid"}
      (concat body
        [[:script {:src "https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js"}]
         [:script {:src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                   :integrity "sha384-Tc5IQib027qvyjSMfHjOMaLkfuWVxZxUPnCJA7l2mCWNIpG9mGCD8wGNIcPD7Txa"
                   :crossorigin "anonymous"}]])]]]))

(define-component :repl-in
  (lambda []
     [:div {:class "repl-in", :style "font-family: Monaco, monospace"}
      [:span {:class "repl-cursor"} (str (current-module-name) ">&nbsp;")]
      [:input {:class "repl-content form-control"
               :type "text"
               :style "border: none; width: 600px; display: inline-block; font-family: Monaco, monospace; font-size: 1em;"}]]))

(define-component :repl-out
  (lambda [content]
     [:div {:class "repl-out", :style "font-family: Monaco, monospace"}
      [:span {:class "repl-cursor"} "= "]
      [:span {:class "repl-content" :style "width: 100%"} content]]]))

(define-function replContent []
  (. (. (js/jQuery ".repl-content") last) val))

(define-function html-encode [s]
  (. (. s replace "<" "&lt;") replace ">" "&gt;"))

(define-function appendOutput [content]
  (. (js/jQuery "#repl") append (html [[:repl-out (html-encode content)] [:repl-in]])))

(define-function focus []
  (. (. (js/jQuery ".repl-content") last) focus))

(define-component :repl
  (lambda
    []
    [:div {:id "repl"}
     [:javascript
      '(. document addEventListener "keydown"
          (function [event]
            (cond (=== event.keyIdentifier "Enter")
                  (do
                    (pbnj.wonderscript.repl.appendOutput
                      (pbnj.wonderscript.inspect
                        (pbnj.wonderscript.readString
                          (pbnj.wonderscript.repl.replContent))))
                    (pbnj.wonderscript.repl.focus)))))]
     [:repl-in]]))

(define-function main []
  (module user)
  (render [:layout [:repl]]))

(main)
