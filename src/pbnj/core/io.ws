; vim: ft=clojure
(module pbnj.core.io)

(define Promise
  (do
    (nodejs?
      (js.node/require "bluebird"))
    (browser? js/Promise)))

(define-macro promise
  [binds &forms]
  (cons 'Promise. (cons 'lambda (cons binds forms))))

(define-macro >>
  [x &forms]
  (cons '..
    (cons x
          (map
            (lambda [form] (list 'then (list 'lambda '[x] (list form 'x))))
            forms))))

(define-function fulfilled?
  [x]
  (.? x isFulfilled))

(define-function rejected?
  [x]
  (.? x isRejected))

(define-function pending?
  [x]
  (.? x isPending))

(define-function canceled?
  [x]
  (.? x isCanceled?))

(browser?
  (define-function http-get
    [url]
    (promise [success error]
      (do-to (js/XMLHttpRequest.)
        (.addEventListener "error" error)
        (.addEventListener "load" success)
        (.open "GET" url)
        (.send nil)))))
