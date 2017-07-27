(require "../src/pbnj/wonderscript/syntax.ws")
(ns examples.syntax)

(alias 's 'pbnj.wonderscript.syntax)

(p (.toJS (s/Nil.)))
(p (.toJS (s/Boolean. true)))
(p (.toJS (s/Boolean. false)))
(p (.toJS (s/Number. 3)))
(p (.toJS (s/String. "adfasdfasd")))
(p (.toJS (s/Keyword. "pbnj.core" "str")))
(p (.toJS (s/Symbol. "pbnj.core" "str")))
(p (.toJS (s/Instant. (js/Date.))))
(p (.toJS (s/Regex. (js/RegExp. "^[A-Za-z0-9]$" "g"))))
