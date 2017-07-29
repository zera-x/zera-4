(ns* examples.imap)

(define *imap-config*
  {:user "iktome@gmail.com"
   :password "7hammers&13nails"
   :host "imap.gmail.com"
   :port 933
   :tls true})

(define Imap (js.node/require "imap"))
(define imap (new Imap (map->object *imap-config*)))

(define proc-mail
  {:message {:body {:data nil
                    :end nil}
             :attributes nil
             :end}
   :error nil
   :end nil})

(define-function on-message
  [msg seqno]
  (say (str "Message #" seqno))
  (let [prefix (str "(#" seqno ")")]
    (.on msg "body"
         (lambda [stream info]
            (let [buffer (array)]
              (.on stream "data" (& (.push buffer (.toString % "utf8"))))
              (.once stream "end" (& (say (str "Header: " (.join buffer ""))))))))
    (.once msg "attributes" )))

(define-function process-inbox
  [err box]
  (if err (throw err))
  (let [f (-> imap .-seq (.fetch "1:3" (map->object {:bodies "HEADER.FIELDS (FROM TO SUBJECT DATE)" :struct true})))]
    (.on f "message" on-message)
    (.once f "error" on-error)
    (.once f "end" on-end)))

(define-function on-ready
  []
  (open-inbox process-inbox))

(.once imap "ready" on-ready)
