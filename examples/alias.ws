(ns examples.alias)

(alias '_ 'pbnj.core)

(is (str 1 2 3) (_/str 1 2 3))
