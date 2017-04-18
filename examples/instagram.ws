; vim: ft=clojure
(require "../src/pbnj/peanutbutter.ws")
(require "../src/pbnj/http-service.ws")
(module hype-stream)
(use pbnj.http-service)

(define- html pbnj.peanutbutter/compile)
(define- define-component pbnj.peanutbutter/define-component)

(define *client-id* "b9bd493916ff449bac5ed7d0f3ddb44f")
(define *access-token* "4366567491.b9bd493.1e3417a1c28b438297fa134dca5d9c66")

(define-function instagram-client
  [id token]
  (let [Instagram (.- (js.node/require "node-instagram") default)
        params (. js/Object (create nil))]
    (.-set! params "clientId" *client-id*)
    (.-set! params "accessToken" *access-token*)
  (new Instagram params)))

(define-function media-item
  [obj]
  (reduce
     (array->list (. js/Object (keys obj)))
     (lambda [m k]
        (let [v (.- obj (symbol k))
              v* (cond (array? v) (array->list v)
                       (object? v) (object->map v)
                       :else v)]
          (assoc m (keyword k) v*)))
     {}))

(define-function media-stream
  [obj]
  (let [data (.- obj data)]
    (map (array->list data) media-item)))

(define-component :instagram/media
  (lambda
    [stream]
    (map stream (lambda [x] [:instagram/media-item x]))))

(define-function fmt-time [t]
  (let [d (new js/Date (* 1 t))]
    (str (+ 1 (. d getUTCMonth)) "/" (. d getUTCDate) "/" (. d getUTCFullYear))))

(define-component :instagram/media-image
  (lambda
    [item]
    (let [img (object->map (get-in item [:images :standard_resolution]))
          txt (get-in item [:caption :text])]
      [:div
       [:a {:href (item :link)}
        [:img {:src (img :url) :width (img :width) :height (img :height)}]]
       [:p (fmt-time (item :created_time))]
       [:p txt]])))

(define-component :instagram/media-item
  (lambda
    [item]
    (println item)
    [:div {:class "instagram-media-item"}
     (cond (= "image" (item :type)) [:instagram/media-image item])]))

(define-function show-profile
  [client]
  (.. client
      (get "users/self")
      (then println)))

(define-function show-media
  ([client]
   (show-media client "self"))
  ([client user]
   (.. client
       (get (str "users/" user "/media/recent"))
       (then media-stream)
       (then (lambda [x] (html [:instagram/media x]))))))

(define *client* (instagram-client *client-id* *access-token*))

(define-service hype-stream
  "A web application to consolidate social media streams"
  (GET "/?" [req]
       (let [params (req :query)
             cid (params :client-id)
             token (params :access-token)
             user (get params :user "self")]
         (if-not (or cid token)
           (throw (new js/Error "client-id and access-token are required")))
         (show-media (instagram-client cid token) user))))

(start hype-stream 4000
       (lambda [] (println "Hype Stream Service - listening at http://localhost:4000")))

; https://www.instagram.com/oauth/authorize/?client_id=b9bd493916ff449bac5ed7d0f3ddb44f&redirect_uri=http://delonnewman.name&response_type=code
; curl -F "client_id=b9bd493916ff449bac5ed7d0f3ddb44f" \
;    -F "client_secret=d936872b5d9a4799a58b61d75f5ea476" \
;    -F "grant_type=authorization_code" \
;    -F "redirect_uri=http://delonnewman.name" \
;    -F "code=564068e87a1b4f7184e8a702fee7cc58" \
;    https://api.instagram.com/oauth/access_token
