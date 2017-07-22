(module examples.macroscope)

(module test1)
(define x 1)

(define-macro tmac
  ([] [x])
  ([y] [x y]))

(p (tmac))
(p (tmac 2))

(module test2)

(p (test1/tmac))
(p (test1/tmac 3))
