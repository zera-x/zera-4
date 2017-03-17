; vim: ft=clojure
(require "src/pbnj/peanutbutter.ws")

(module pbnj.wonderscript.repl)

(define- html pbnj.peanutbutter/html)
(define- html-encode pbnj.peanutbutter/html-encode)
(define- define-component pbnj.peanutbutter/define-component)
(define- render pbnj.peanutbutter/render)

(define-component ::layout
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

(define-component ::repl-in
  (lambda []
     [:div {:class "repl-in", :style {:font-family "Monaco, monospace"}}
      [:span {:class "repl-cursor"} (str (current-module-name) "> ")]
      [:input {:class "repl-content"
               :type "text"
               :autocomplete "off"
               :autocorrect "off"
               :autocapitalize "off"
               :spellcheck "false"
               :style {:border :none
                       :margin-top 5
                       :margin-bottom 0
                       :width 800
                       :display :inline-block
                       :font-family "Monaco, monospace"
                       :font-size "1em"}}]]))

(define-component ::repl-out
  (lambda [content]
     [:div {:class "repl-out", :style {:font-family "Monaco, monospace"}}
      [:span {:class "repl-content" :style "width: 100%"} content]]]))

(define-component ::display-out (always [:div {:class "display-out"}]))

(define-function replContent []
  (.. (js/jQuery ".repl-content") last val))

(define-function appendOutput [content]
  (. (js/jQuery "#repl")
     (append
       (html [[::repl-out (fmt-output content)]
              [::repl-in]
              [::display-out]]))))

(define-function setFocus []
  (.. (js/jQuery ".repl-content") last focus))

(define-function setContent [x]
  (.. (js/jQuery ".repl-content") last (val x)))

(define *history* '())
(define *scroll-history* '())

(define-function history [] *history*)

(define-function pushHistory [x]
  (set! *history* (conj *history* x)))

(define-function popHistory []
  (set! *history* (rest *history*)))

(define-function pushScrollHistory [x]
  (set! *scroll-history* (conj *scroll-history* x)))

(define-function popScrollHistory []
  (set! *scroll-history* (rest *scroll-history*)))

(define-function scrollHistoryBack []
  (if-not (empty? *history*)
    (let [exp (first *history*)]
      (pushScrollHistory exp)
      (popHistory)
      exp)))

(define-function scrollHistoryForward []
  (if-not (empty? *scroll-history*)
    (let [exp (first *scroll-history*)]
      (pushHistory exp)
      (popScrollHistory)
      exp)))

(define *output-count* 0)

(define-function output-var []
  (symbol (str "$" *output-count*)))

(define-function evalInput []
  (let [in (replContent)]
    (try
      (pushHistory in)
      (set! *output-count* (+ 1 *output-count*))
      (let [out (pbnj.wonderscript/readString in)
            sym (output-var)]
        (eval (list 'do
                    (list 'use (.- pbnj/MODULE_SCOPE "@@NAME@@"))
                    (list 'define sym (list 'quote out))))
        [:success (inspect out)])
      (catch [e js/Error]
        [:error e]))))

(define-function fmt-output [out]
  (if (= (first out) :error)
    (do
      (.. (js/jQuery ".repl-content") last (addClass "has-error"))
      [:div {:class "alert alert-danger" :style {:margin-bottom 3}}
        (html-encode
          (.? (.- (second out) message)
              (replace (new js/RegExp "\n" "g") "<br>")))])
    [:div (output-var) " = " (html-encode (second out))]))

(define-function display [&xs]
  (.. (js/jQuery ".display-out") last (append (apply html xs)))
  nil)

(define-function pprint [exp]
  (display [(inspect exp) [:br]]))

(define-component ::repl
  (lambda []
    [:div {:id "repl"}
     [:javascript
      '(. document addEventListener "keydown"
          (function [event]
            (cond (=== event.keyIdentifier "Enter")
                    (do
                      (pbnj.wonderscript.repl.appendOutput
                        (pbnj.wonderscript.repl.evalInput))
                    (pbnj.wonderscript.repl.setFocus))
                  (=== event.keyIdentifier "Up")
                    (pbnj.wonderscript.repl.setContent
                      (pbnj.wonderscript.repl.scrollHistoryBack))
                  (=== event.keyIdentifier "Down")
                    (pbnj.wonderscript.repl.setContent
                      (pbnj.wonderscript.repl.scrollHistoryForward)))))]
     [::repl-in]
     [::display-out]]))

(define-function main []
  (module pbnj.user)
  (define display pbnj.wonderscript.repl/display)
  (render [:pbnj.wonderscript.repl/layout [:pbnj.wonderscript.repl/repl]]))

(main)
