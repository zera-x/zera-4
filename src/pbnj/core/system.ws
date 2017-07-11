; vim: ft=clojure
(module pbnj.core.system)

(nodejs?

  (define
    {:doc "An alias for process.abort() on Node.js"
     :platforms #{:nodejs}
     :added "1.0"}
    abort process/abort)

  (define 
    {:doc "An alias for process.chdir() on Node.js"
     :arglists '([directory])
     :platforms #{:nodejs}
     :added "1.0"}
    chdir process/chdir)

  (define
    {:doc "An alias for process.cwd() on Node.js"
     :arglists '([])
     :platforms #{:nodejs}
     :added "1.0"}
    cwd process/cwd)

  (define
    {:doc "An alias for process.exit() on Node.js"
     :arglists '([] [code])
     :platforms #{:nodejs}
     :added "1.0"}
    exit process/exit)

  (define- *fs* (js.node/require "fs"))

  ;; TODO: Make a browser version of slurp and spit

  (define-function slurp
    "Read entire contents of `file` to a string"
    {:added "1.0"}
    [file]
    (-> (.readFileSync *fs* file) .toString))

  (define-function spit
    "Write `data` to `file`. Data can be a String, Buffer, or Uint8Array."
    {:added "1.0"}
    [file data]
    (.writeFileSync *fs* file data) file)

  (define- *path* (js.node/require "path"))

  (define
    {:doc "An alias for path.basename() on Node.js"
     :arglists '([path] [path ext])
     :platforms #{:nodejs}
     :added "1.0"}
    basename (.-basename *path*))

  (define
    {:doc "An alias for path.dirname() on Node.js"
     :arglists '([path] [path ext])
     :platforms #{:nodejs}
     :added "1.0"}
    dirname (.-dirname *path*))

  (define
    {:doc "An alias for path.extname() on Node.js"
     :arglists '([path] [path ext])
     :platforms #{:nodejs}
     :added "1.0"}
    extname (.-extname *path*))
)
