; vim: ft=clojure
(require "src/pbnj/peanutbutter.ws")

(module pbnj.wonderscript.repl)

(define- html pbnj.peanutbutter/html)
(define- html-encode pbnj.peanutbutter/html-encode)
(define- define-component pbnj.peanutbutter/define-component)
(define- render pbnj.peanutbutter/render)

; TODO: add error reporting and history, need to add try/catch blocks to WonderScript

(define-component :layout
  (lambda
    [&body]
  [:html {:lang "en"}
   [:head
    [:title "WonderScript Shell"]
    [:meta {:charset "utf-8"}]
    [:meta {:http-equiv "X-UA-Compatible" :content "IE=edge"}]
    [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
    [:link {:rel "stylesheet"
            :href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
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
     [:div {:class "repl-in", :style {:font-family "Monaco, monospace"}}
      [:span {:class "repl-cursor"} (str (current-module-name) "> ")]
      [:input {:class "repl-content"
               :type "text"
               :autocomplete "off"
               :autocorrect "off"
               :style {:border :none
                       :margin-top 5
                       :margin-bottom 0
                       :width 800
                       :display :inline-block
                       :font-family "Monaco, monospace"
                       :font-size "1em"}}]]))

(define-component :repl-out
  (lambda [content]
     [:div {:class "repl-out", :style {:font-family "Monaco, monospace"}}
      [:span {:class "repl-content" :style "width: 100%"} content]]]))

(define-function replContent []
  (. (. (js/jQuery ".repl-content") last) val))

(define-function appendOutput [content]
  (println content)
  (. (js/jQuery "#repl") append (html [[:repl-out (fmt-output content)] [:repl-in]])))

(define-function focus []
  (. (. (js/jQuery ".repl-content") last) focus))

(define-function evalInput []
  (let [in (replContent)]
    (try
      [:success (pbnj.wonderscript/readString in)]
      (catch [e js/Error]
        [:error e]))))

(define-function fmt-output [out]
  (if (= (first out) :error)
    [:div {:class "alert alert-danger" :style {:margin-bottom 3}}, (.- (second out) message)]
    [:div "= " (inspect (second out))]))

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
                      (pbnj.wonderscript.repl.evalInput))
                    (pbnj.wonderscript.repl.focus)))))]
     [:repl-in]]))

(define-function main []
  (module pbnj.user)
  (render [:layout [:repl]]))

(main)
