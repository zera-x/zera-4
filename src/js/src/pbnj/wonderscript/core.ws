; vim: ft=clojure

(def sq (lambda [x] (* x x)))

(def str/join
  (lambda [col, delim]
          (reduce col
                  (lambda [s x]
                          (str s delim x)))))

(def map/entries ->Array)

(def pprint
  (lambda [exp]
          (cond (number? exp) (str exp)
                (string? exp) (str "\"" exp "\"")
                (isSymbol exp) (str exp)
                (isKeyword exp) (str exp)
                (boolean? exp) (cond exp "true" :else "false")
                (nil? exp) "nil"
                (date? exp) (str "#inst \"" exp "\"")
                (regexp? exp) (str "#\"" exp "\"")
                (isMap exp)
                    (str "{" (str/join (map (apply concat (map/entries exp)) pprint) " ") "}")
                (isCollection exp)
                  ((lambda [delims]
                    (str (nth delims 0) (str/join (map exp pprint) " ") (nth delims 1)))
                   (cond (isVector exp) ["[" "]"]
                         (isSet exp) ["#{" "}"]
                         :else ["'(" ")"]))
                 :else (str exp)))) 

(def compile
  (lambda [exp]
          (cond (number? exp) (str exp)
                (string? exp) (str "\"" exp "\"")
                (isSymbol exp) (str "pbnj.core.symbol(" exp ")")
                (isKeyword exp) (str "pbnj.core.keyword(" exp ")")
                (boolean? exp) (cond exp "true" :else "false")
                (nil? exp) "null"
                (date? exp) (str "new Date(\"" exp "\")")
                (regexp? exp) (str "/" exp "/")
                (isMap exp)
                    (str "pbnj.core.hashMap(" (str/join (map (apply concat (map/entries exp)) pprint) ", ") ")")
                (isCollection exp)
                  ((lambda [delims]
                    (str (nth delims 0) (str/join (map exp pprint) " ") (nth delims 1)))
                   (cond (isVector exp) ["pbnj.core.vector(" ")"]
                         (isSet exp) ["pbnj.core.set(" ")"]
                         :else ["pbnj.core.list(" ")"]))
                 :else (str exp)))) 

(defmacro let [exp]
  (def bindings (pair (->Array (second exp))))
  (list 'apply (list 'lambda
                     (into [] (map bindings first))
                     (second (rest exp)))
        (into [] (map bindings second))))

(defmacro do [exp]
  (list 'apply (cons 'lambda (cons [] (rest exp)))))

(defmacro if [exp]
  (def r (rest exp))
  (def pred (first r))
  (def conse (second r))
  (def alt (second (rest r)))
  (cond alt (list 'cond pred conse :else alt)
        :else (list 'cond pred conse)))

(defmacro defn [exp]
  (def r (rest exp))
  (list 'def (first r) (cons 'lambda (cons (second r) (rest (rest r))))))

(defmacro defonce [exp]
  (def r (rest exp))
  (if (not (defined? (first r)))
    (list 'def (first r) (second r))
    :defined))

(defmacro defstruct [exp]
  (def name (second exp))
  (def args (rest (rest exp)))
  (list 'def name (cons 'struct (cons (keyword (str name)) args)))) 
