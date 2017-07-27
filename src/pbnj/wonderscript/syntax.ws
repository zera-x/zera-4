(ns pbnj.wonderscript.syntax)

(define-protocol Syntax
  (toJS [self]))

;(define-function syntax?
;  [x]
;  (isa? x Syntax))

(define-protocol Literal Syntax)

;(define-function literal?
;  [x]
;  (isa? x Literal))

(define-protocol Atomic Literal)

;(define-function atomic?
;  [x]
;  (isa? x Atomic))

(define-type Nil
  []
  Atomic
  (toJS [self] "null"))

;(define-function nil?
;  [x]
;  (isa? x Nil))

(define-type Boolean
  [value]
  Atomic
  (toJS [self] (if (.-value self) "true" "false")))

;(define-function boolean?
;  [x]
;  (isa? x Boolean))

(define-protocol Numeric)

;(define-function numeric?
;  [x]
;  (isa? x Numeric))

(define-type Number
  [value]
  Atomic
  Numeric
  (toJS [self] (str (.-value self))))

;(define-function number?
;  [x]
;  (isa? x Number))

(define-type String
  [value]
  Atomic
  (toJS [self] (str "'" (.-value self) "'")))

(define-protocol Symbolic)

;(define-function symbolic?
;  [x]
;  (isa? x Symbolic))

(define-type Keyword
  [ns name]
  Atomic
  Symbolic
  (toJS [self]
        (str "_.keyword(" (.toJS (.-ns self)) ", " (.toJS (.-name self)) ")")))

;(define-function keyword?
;  [x]
;  (isa? x Keyword))

(define-type Symbol
  [ns name]
  Atomic
  Symbolic)

;(define-function symbol?
;  [x]
;  (isa? x Symbol))

(define-type Instant
  [value]
  Atomic)

;(define instant? (lambda [x] (isa? x Instant)))

(define-type RegExp
  [value]
  Atomic)

;(define regexp?
;  (lambda
;    [x]
;    (isa? x RegExp)))

(define-protocol Collection Literal)

;(define-function collection?
;  [x]
;  (isa? x Collection))

(define-type List
  [value]
  Collection)

;(define-function list?
;  [x]
;  (isa? x List))

(define-type Map
  [value]
  Collection
  (toJS [self]
        (str "_.hashMap("
          (->> (.-value self)
               (map (lambda [x] (str (.toJS (x 0)) ", " (.toJS (x 1)))))
               (reduce (lambda [s x] (str s ", " x)))) ")")))

;(define-function map?
;  [x]
;  (isa? x Map))

(define-type Vector
  [value]
  Collection
  (toJS [self]
        (str "_.vector("
          (->> (.-value self)
               (map (lambda [x] (.toJS x)))
               (reduce (lambda [s x] (str s ", " x)))) ")")))

;(define-function vector?
;  [x]
;  (isa? x Vector))

(define-type Set
  [value]
  Collection
  (toJS [self]
        (str "_.set(["
          (->> (.-value self)
               (map (lambda [x] (.toJS x)))
               (reduce (lambda [s x] (str s ", " x)))) "])")))

;(define-function set?
;  [x]
;  (isa? x Set))

(define-type Variable
  [ns name]
  Syntax
  (toJS [self] (str (.-ns self) "." (.-name self))))

;(define-function variable?
;  [x]
;  (isa? x Variable))

(define-type QuotedValue
  [value]
  Syntax
  (toJS
    [self]
    (str "_.identity(" (.toJS (.-value self)) ")")))

;(define-function quoted-value?
;  [x]
;  (isa? x QuotedValue))

(define-type Definition
  [name value meta]
  Syntax)

;(define-function definition?
;  [x]
;  (isa? x Definition))

(define-type If
  [predicate alternate consequent]
  Syntax
  (toJS
    [self]
    (if (.-consequent self)
      (str "(" (.toJS (.-predicate self)) ")?(" (.toJS (.-alternate self)) "):(" (.toJS (.-consequent self)) ")")
      (str "(" (.toJS (.-predicate self)) ")?(" (.toJS (.-alternate self)) "):null"))))

;(define-function conditional?
;  [x]
;  (isa? x Conditional))

(define-type Lambda
  [arglists bodies]
  Syntax
  (toJS [self] (lambda->js (.-arglists self) (.-bodies self))))

(define-function lambda-bodies->js
  [args body]
  (p args)
  (p body)
  (str (.toJS args) " " (.toJS body)))


(define-function lambda->js
  [arglists bodies]
  (->> (zipmap arglists bodies)
       (map (lambda [x] (apply lambda-bodies->js x)))))
;    (join
;      (concat
;        (map-indexed
;          (lambda [i x]
;            (let [args (x 0)
;                  body (x 1)
;                  kw (if (= i 0) "if" "else if")]
;              (str kw " (arguments.length === " (count args) "){ return " (.toJS body) "; }"))) xs)
;        ["else { throw new Error('wrong number of arguments') }"])
;      "\n")))

;(define-function lambda?
;  [x]
;  (isa? x Lambda))

(define-type Block
  [expressions]
  Syntax)

;(define-function block?
;  [x]
;  (isa? x Block))

(define-type TryBlock
  [body catchClause finallyClause]
  Syntax)

;(define-function try-block?
;  [x]
;  (isa? x TryBlock))

(define-type CatchClause
  [bindings body]
  Syntax)

;(define-function catch-clause?
;  [x]
;  (isa? x CatchClause))

(define-type FinallyClause
  [body]
  Syntax)

;(define-function finally-clause?
;  [x]
;  (isa? x FinallyClause))

(define-type Loop
  [bindings body]
  Syntax)

;(define-function loop?
;  [x]
;  (isa? x Loop))

(define-type RecursionPoint
  [args]
  Syntax)

;(define-function recursion-point?
;  [x]
;  (isa? x RecursionPoint))

(define-type Assignment
  [obj value]
  Syntax)

;(define-function assignment?
;  [x]
;  (isa? x Assignment))

(define-type ProtocolDefinition
  [name specs meta]
  Syntax)

;(define-function protocol-definition?
;  [x]
;  (isa? x ProtocolDefinition))

(define-type TypeDefinition
  [name bindings specs meta]
  Syntax)

;(define-function type-definition?
;  [x]
;  (isa? x TypeDefinition))

(define-type MacroDefinition
  [name bindings body meta]
  Syntax)

;(define-function macro-definiation?
;  [x]
;  (isa? x MacroDefinition))

(define-type ThrownException
  [expression]
  Syntax)

;(define-function thrown-exception?
;  [x]
;  (isa? x ThrownException))

(define-type PropertyAccessor
  [obj property]
  Syntax)

;(define-function property-accessor?
;  [x]
;  (isa? x PropertyAccessor))

(define-type PropertyAssignment
  [obj property value]
  Syntax)

;(define-function property-assignment?
;  [x]
;  (isa? x PropertyAssignment))

(define-type MethodApplication
  [obj method args]
  Syntax)

;(define-function method-application?
;  [x]
;  (isa? x MethodApplication))

(define-type ClassInstantiation
  [class args]
  Syntax)

;(define-function class-instantiation?
;  [x]
;  (isa? x ClassInstantiation))

(define-type ModuleDefinition
  [name]
  Syntax)

;(define-function method-definition?
;  [x]
;  (isa? x MethodDefinition))

(define-type ModuleRequire
  [name]
  Syntax)

;(define-function module-require?
;  [x]
;  (isa? x ModuleRequire))

(define-type ModuleSet
  [name]
  Syntax)

;(define-function module-set?
;  [x]
;  (isa? x ModuleSet))

(define-type Application
  [fn args]
  Syntax)

;(define-function application?
;  [x]
;  (isa? x Application))
