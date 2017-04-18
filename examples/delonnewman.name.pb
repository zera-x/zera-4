; vim: ft=clojure
(define layout [&body]
    [:html {:lang "en"}
     [:head
      [:title "Delon R. Newman - Programmer. Problem Solver."]
      [:meta {:charset "utf-8"}]
      [:meta {:http-equiv "X-UA-Compatible" :content "IE=edge"}]
      [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
      [:meta {:name "author" :content "Delon Newman <contact@delonnewman.name"}]
      [:meta {:name "keywords" :content "software engineer engineering programmer programming portfolio javascript clojure ruby php java software developer web developer application developer systems programmer unix linux"}]
      [:link {:rel "stylesheet"
              :href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
              :integrity "sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u"
              :crossorigin "anonymous"}]
      [:link {:rel "stylesheet"
              :href "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.1/css/font-awesome.min.css"}]
      [:link {:href "https://fonts.googleapis.com/css?family=Crimson+Text:400,400italic,600italic"
              :rel "stylesheet"
              :type "text/css"}]
      [:link {:href "http://fonts.googleapis.com/css?family=Ubuntu+Mono"
              :rel "stylesheet"
              :type "text/css"}]]
     [:css
        [:body {:font-family ["Crimson Text", "serif"]
               :font-size "1.2em"
               :background-color "#eee8d5"}]] 
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

(layout [:php= $_GET])
