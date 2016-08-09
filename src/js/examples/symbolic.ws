; vim: set ft=clojure
; Joseph McCarthy's diff function
; see:
;   J. McCarthy, Recursive Functions of Symbolic Expressions and Their Computation by Machine, Part I, April 1960
; 
; diff [y; x] = [atom[y] →
;                 [eq[y; x] → ONE; T → ZERO];
;                eq[car [Y]; PLUS] → cons [PLUS; maplist[cdr[y]; λ[[z]; diff[car [z]; x]]]];
;                eq[car [y]; TIMES] → cons[PLUS; maplist[cdr[y];
;                                                        λ[[z]; cons [TIMES; maplist[cdr [y];
;                                                                                    λ[[w];
;                                                                                      ¬ eq [z; w] → car [w];
;                                                                                      T → diff [car [[w]; x]]]]]]]
; Subtraction and exponents are an addition

;(module 'pbnj.examples)

(define-function diff [y x]
  (cond (or (nil? y) (nil? x))
          (throw (str "x and y cannot be nil"))
        (symbol? y)
          (if (= y x) 1 0)
        (or (= (first y) '+) (= (first y) '-))
          (cons (first y)
                (map (rest y)
                     (lambda [z] (diff z x))))
        (= (first y) '*)
          (let [values (rest y)]
            (cons '+ (map values
                          (lambda [z]
                                  (cons '* (map values
                                                (lambda [w]
                                                        (if (not= z w)
                                                            w
                                                            (diff w x)))))))))
        (= (first y) 'pow)
          (let [var (second y)
                exp (second (rest y))]
            (list '* exp (list 'pow var (list '- exp 1))))
        :else
          (throw (str "'" y "' is not a valid expression"))))

(define expressions '[x
                      y
                      (+ x y)
                      (+ x (- a y))
                      (* x y)
                      (* x (+ x a) y)
                      (pow x a)
                      (+ (* a x) (pow x b))])

(each
  (map expressions
       (lambda [exp] (str exp " => " (diff exp 'x))))
  println)
