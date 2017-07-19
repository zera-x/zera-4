; vim: ft=clojure
(module examples.glucose)

(require "../src/pbnj/honey.ws")
(require "../src/pbnj/http-service.ws")
(require "../src/pbnj/peanutbutter.ws")

(use pbnj.honey)

(define- html pbnj.peanutbutter/compile)
(define- *db* (connect "sqlite://examples/TrueMgrAirBackup 2017-06-25.ndm"))

(define-function fmt-date
  [d]
  (str (.getUTCFullYear d) "-"
       (.getUTCMonth d) "-"
       (.getUTCDate d) " @ "
       (.getUTCHours d) ":"
       (.getUTCMinutes d)))

(define-function table
  [res]
  [:table
   [:thead
    [:tr
     [:th "t"]
     [:th "bg"]]]
   [:tbody
    (map
      (lambda [row]
              [:tr
               [:td (fmt-date (row 0))]
               [:td (row 1) " mg/dL"]]) res)]])

(.all *db* "SELECT * FROM Results"
       (lambda [err res]
               (->> res
                    (map object->map)
                    (remove (lambda [r] (>= 1 (r :result_GlucoseConcentration))))
                    (map (lambda [r] [(js/Date. (* 1000 (r :result_Timestamp))) (r :result_GlucoseConcentration)]))
                    (sort-by first)
                    table
                    html
                    say)))
