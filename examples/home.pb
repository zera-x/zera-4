; vim: ft=clojure
(define layout [&body]
    [:html {:lang "en"}
     [:head
      [:title "Home"]
      [:meta {:charset "utf-8"}]
      [:meta {:http-equiv "X-UA-Compatible" :content "IE=edge"}]
      [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
      [:link {:rel "stylesheet"
              :href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
              :integrity "sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u"
              :crossorigin "anonymous"}]]
     [:body
      [:div {:class "container-fluid", :style {:margin-top "10px"}}
        (concat body
          [[:script {:src "https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js"}]
           [:script {:src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                     :integrity "sha384-Tc5IQib027qvyjSMfHjOMaLkfuWVxZxUPnCJA7l2mCWNIpG9mGCD8wGNIcPD7Txa"
                     :crossorigin "anonymous"}]])]]])

(define links
  [:ul
   [:li
    [:a {:href "http://wol.jw.org" :target "__wol"} "Watchtower Online Library"]]])

(layout [:php (print "Hello")])
