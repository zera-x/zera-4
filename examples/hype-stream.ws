; vim: ft=clojure
(require "../src/zera/core/html.zera")
(require "../src/zera/core/http-service.zera")
(ns hype-stream)
(use zera.core.http-service)

(def html zera.core.html)

(def client-id "b9bd493916ff449bac5ed7d0f3ddb44f")
(def access-token "4366567491.b9bd493.1e3417a1c28b438297fa134dca5d9c66")
(def dbconn (atom nil))

; Stream Item
; {:item/date js/Date
;  :item/type keyword
;  :item/content map}

; deps: promise-mysql, node-instagram, feedparser-promised

(def schema-code "CREATE SCHEMA IF NOT EXISTS `hype-stream`")
(def table-code
  "CREATE TABLE IF NOT EXISTS `items` (
    `id` BIGINT,
    `t` TIMESTAMP NOT NULL,
    `type` VARCHAR(50) NOT NULL,
    `content` LONGBLOB NOT NULL,
    INDEX (`t`, `type`),
    PRIMARY KEY (`id`)
  ) ENGINE=INNODB;")

(defn initdb!
  []
  (let [mysql (js.node/require "promise-mysql")]
    (.then (.createConnection mysql (->js {:host "localhost" :user "root" :password "123" :database "hype-stream"}))
        (fn [conn]
            (reset! dbconn conn)
            (.query conn schema-code)
            (.query conn table-code)))))

(defn add-item!
  [item]
  (.query @dbconn
          "INSERT INTO `items` (`id`, `t`, `type`, `content`) VALUES (?, ?, ?, ?)"
          (array (.now js/Date) (item :item/t) (str (item :item/type)) (js/encode-json (item :item/content)))))

(defn update-item!
  [item]
  (if-not (item :item/id)
    (throw "An :item/id is required to perform update"))
  (.query @dbconn
          "UPDATE `items` SET `t` = ?, `type` = ?, `content` = ? WHERE `id` = ?"
          (array (item :item/t) (str (item :item/type)) (js/encode-json (item :item/content)) (item :item/id))))

(defn get-item!
  [id]
  (.query @dbconn
          "SELECT `id`, `t`, `type`, `content` FROM `items` WHERE `id` = ?"
          (array (item :item/id))))

(defn item
  ([t type content]
   (item nil t type content))
  ([id t type content]
   {:item/id id
    :item/t t
    :item/type type
    :item/content content}))

(defn instagram-client
  [id token]
  (let [Instagram (.- (js.node/require "node-instagram") default)]
    (new Instagram (->js {:clientId id, :accessToken token}))))

(defn stackoverflow-feed
  [id]
  (let [parser (js.node/require "feedparser-promised")]
    (.. parser
        (parse (str "https://stackoverflow.com/feeds/user/" id))
        (catch (fn [err] (console.error err))))))

(defn media-item
  [obj]
  (reduce
     (fn [m k]
        (let [v (.- obj (symbol k))
              v* (cond (array? v) (array->list v)
                       (object? v) (object->map v)
                       :else v)]
          (assoc m (keyword k) v*)))
     {}
     (array->list (. js/Object (keys obj)))))

(defn media-stream
  [obj]
  (let [data (.- obj data)]
    (map media-item (array->list data))))

(defn instagram-media
  [stream]
  (map (fn [x] [instagram-media-item x]) stream))

(defn fmt-time [t]
  (let [d (new js/Date (* 1000 t))]
    (str (+ 1 (.getUTCMonth d)) "/" (.getUTCDate d) "/" (.getUTCFullYear d))))

(defn instagram-media-image
  [item]
  (let [img (object->map (get-in item [:images :standard_resolution]))
        txt (get-in item [:caption :text])]
    [:div
      [:a {:href (item :link)}
       [:img {:src (img :url) :width (img :width) :height (img :height)}]]
      [:p (fmt-time (item :created_time))]
      [:p txt]]))

(defn instagram-media-item
  [item]
  (println item)
  [:div {:class "instagram-media-item"}
   (cond (= "image" (item :type)) [instagram-media-image item])])

(defn show-profile
  [client]
  (.. client
      (get "users/self")
      (then println)))

(defn show-media
  ([client]
   (show-media client "self"))
  ([client user]
   (>> (.get client (str "users/" user "/media/recent"))
       media-stream
       (fn [x] (html [instagram-media x])))))

(defservice hype-stream
  "A web application to consolidate social media streams"
  (GET "/?" [req]
       (let [params (req :query)
             cid (get params :client-id client-id)
             token (get params :access-token access-token)
             user (get params :user "self")]
         (show-media (instagram-client cid token) user))))

(defn instagram-feed
  [user]
  (.get (instagram-client client-id access-token)
        (str "users/" user "/media/recent")))

(defn store-instagram-items
  [data]
  (doeach [x (.-data data)]
    (println x)
    (add-item! (item (js/Date.) "instagram" x))))

(defn store-feed
  [type promise]
  (.then promise store-instagram-items))

(defn start-service
  [key ref old new]
  (if (and (nil? old) new)
    (start hype-stream 4000
           (fn [] (println "Hype Stream Service - listening at http://localhost:4000")))))

(defn store-feeds
  [key ref old new]
  (if (and (nil? old) new)
    (store-feed :feed/instagram (instagram-feed "self"))))

(initdb!)
(add-watch dbconn "main" start-service)
;(add-watch dbconn "main" store-feeds)

; https://www.instagram.com/oauth/authorize/?client_id=b9bd493916ff449bac5ed7d0f3ddb44f&redirect_uri=http://delonnewman.name&response_type=code
; curl -F "client_id=b9bd493916ff449bac5ed7d0f3ddb44f" \
;    -F "client_secret=d936872b5d9a4799a58b61d75f5ea476" \
;    -F "grant_type=authorization_code" \
;    -F "redirect_uri=http://delonnewman.name" \
;    -F "code=564068e87a1b4f7184e8a702fee7cc58" \
;    https://api.instagram.com/oauth/access_token
