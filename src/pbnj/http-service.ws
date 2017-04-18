;; vim: ft=clojure
(module pbnj.http-service)

;; FIXME: SCOPING BUG 
;; Without module declaration above *module-name* is still pbnj.core where as (current-module-name) is pbnj.user
;; causes defined variables to come up "undefined"
;(println *module-name*)

(define- request-keys
  {:baseUrl :base-url
   :body :body
   :cookies :cookies
   :fresh :fresh
   :hostname :hostname
   :ip :ip
   :ips :ips
   :method :method
   :originalUrl :original-url
   :params :params
   :path :path
   :protocol :protocol
   :query :query
   :secure :secure
   :stale :stale
   :subdomains :subdomains
   :xhr :xhr
   :url :url
   :res :res
   :headers :headers
   :rawHeaders :raw-headers
   :domain :domain
   :readable :readable
   :statusCode :status-code
   :statusMessage :status-message
   :httpVersionMajor :http-version-major
   :httpVersionMinor :http-version-minor
   :httpVersion :http-version})

(define- request-xformers
  {:params object->map
   :headers object->map
   :raw-headers array->vector
   :query object->map
   :subdomains array->list
   :ips array->list})

(define-function build-request-map
  [req]
  (into {}
        (map (lambda [xs]
                     (let [k (request-keys (xs 0))
                           xfr (request-xformers k)
                           v (xs 1)]
                       [k (if xfr (xfr v) v)]))
             (object->list req request-keys))))

(define-macro define-service
  ([nm &body] (cons 'define-service (cons nm (cons nil body))))
  ([nm doc &body]
   (list 'define {:doc doc :tag :http-service/express} nm
         (cons 'do
               (cons
                 (list 'define- '*http-express* (list 'js.node/require "express"))
                 (cons
                   (list 'define- '*http-express-app* (list '*http-express*))
                   (concat body ['*http-express-app*])))))))

(define-function response [res val]
  (if (and (object? val) (.- val then))
    (. val (then (lambda [x] (. res (send x)))))
    (. res (send val))))

(define-function- build-http-verb
  [verb path bindings body]
  (list '. '*http-express-app*
        (list verb path
              (list 'lambda '[*request* *response* o]
                    (list 'pbnj.http-service/response
                          '*response*
                          (list (cons 'lambda (cons bindings body))
                                (list 'pbnj.http-service/build-request-map '*request*)))))))

(define-function start
  ([svc port]
   (. svc (listen port)))
  ([svc port fn]
   (. svc (listen port fn))))

(define-macro GET
  [path bindings &body]
  (build-http-verb 'get path bindings body))

(define-macro POST
  [path bindings &body]
  (build-http-verb 'post path bindings body))

(define-macro PUT
  [path bindings &body]
  (build-http-verb 'put path bindings body))

(define-macro DELETE
  [path bindings &body]
  (build-http-verb 'delete path bindings body))
