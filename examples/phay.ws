(require "../src/pbnj/phay.ws")
(require "../src/pbnj/peanutbutter.ws")

(module examples.phay)

(define php pbnj.phay/compile)
(define html pbnj.peanutbutter/compile)

(println (php 1))
(println (php "afasdf"))
(println (php :this.is.a/test))
(println (php 'this.is.a/test))
(println (php '$a))
(println (php true))
(println (php {"a" 1}))
(println (php [1 2 3]))
(println (php '(cond true 1 false 2 :else 3)))
(println (php '(if true 1 2)))
(println (php '(? nil)))
(println (php '(<? $a)))
(println (php '(<?php $a)))
(println (php '(cast string true)))
(println (php '(do (print 1) (print 2))))
(println (php '(const x 1)))
(println (php '(var $x 1)))
(println (php '(public $x 1)))
(println (php '(private $x)))
(println (php '(protected $x 1)))
(println (php '(function [$x] (return $x))))
(println (php '(throw (new Exception "this is a test"))))
(println (php '(do (set! $i 0) (while (< $i 10) (print $i) (set! $i (+ $i 1))))))
(println (html [:html
                [:head
                  [:title "Hello PHP"]]
                [:body
                  [:a {:onclick '(alert "Hello")} "Click Me!"]
                  [:php '(print "Hello, World")]]]))
