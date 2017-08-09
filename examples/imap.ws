(ns examples.imap)

(def *imap-config*
  {:user "iktome@gmail.com"
   :password "7hammers&13nails"
   :host "imap.gmail.com"
   :port 933
   :tls true})

(def Imap (js.node/require "imap"))
(def imap (new Imap (map->object *imap-config*)))

(def proc-mail
  {:message {:body {:data nil
                    :end nil}
             :attributes nil
             :end}
   :error nil
   :end nil})

(defn on-message
  [msg seqno]
  (say (str "Message #" seqno))
  (let [prefix (str "(#" seqno ")")]
    (.on msg "body"
         (fn [stream info]
            (let [buffer (array)]
              (.on stream "data" (& (.push buffer (.toString % "utf8"))))
              (.once stream "end" (& (say (str "Header: " (.join buffer ""))))))))
    (.once msg "attributes" )))

(defn process-inbox
  [err box]
  (if err (throw err))
  (let [f (-> imap .-seq (.fetch "1:3" (map->object {:bodies "HEADER.FIELDS (FROM TO SUBJECT DATE)" :struct true})))]
    (.on f "message" on-message)
    (.once f "error" on-error)
    (.once f "end" on-end)))

(defn on-ready
  []
  (open-inbox process-inbox))

(.once imap "ready" on-ready)
