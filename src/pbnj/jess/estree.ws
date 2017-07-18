(module pbnj.jess.estree)

(define-function node?
  [x]
  (.-type x))

(define statement? node?)

(define declaration? statement?)

(define expression? node?)

(define pattern? node?)

(define-function source-location?
  [x]
  (and (.-source x) (.-start x) (.-end x)))

(define-function position?
  [x]
  (and (.-line x) (.-column x)))

(define-function identifier?
  [x]
  (and (node? x) (= (.-type x) "Identifier")))

(define-function identifier->jess
  [x]
  (symbol (.-name x)))

(define pattern->jess identifier->jess)

(define-function literal?
  [x]
  (and (node? x) (= (.-type x) "Literal")))

(define-function literal->jess
  [x]
  (.-value x))

(define-function regexp-literal?
  [x]
  (and (literal? x) (.-regex x)))

(define-function regexp->jess
  [x]
  (list 'new 'RegExp (..? x regex pattern) (..? x regex flags)))

(define-function program?
  [x]
  (and (node? x) (= (.-type x) "Program")))

(define-function program->jess
  [x]
  (cons 'do
        (map statement->jess (.-body x))))

(define-function statement->jess
  [x]
  (cond (expression-statement? x) (expression-statement->jess x)
        (block-statement? x) (block-statement->jess x)
        ;(empty-statement? x) (empty-statement->jess x)
        ;(debugger-statement? x) (debugger-statement->jess x)
        ;(with-statement? x) (with-statement->jess x)
        (return-statement? x) (return-statement->jess x)
        ;(labeled-statement? x) (labeled-statement->jess x)
        (break-statement? x) (break-statement->jess x)
        (continue-statement? x) (continue-statement->jess x)
        (if-statement? x) (if-statement->jess x)
        ;(switch-statement? x) (switch-statement->jess x)
        (throw-statement? x) (throw-statement->jess x)
        (try-statement? x) (try-statement->jess x)
        (while-statement? x) (while-statement->jess x)
        (do-while-statement? x) (do-while-statement->jess x)
        (for-statement? x) (for-statement->jess x)
        (for-in-statement? x) (for-in-statement->jess x)
        (program? x) (program->jess x)
        (declaration? x) (declaration->jess x)
        :else
          (do
            (println x)
            (throw (str "invalid statement: " (.-type x))))))

(define-function declaration->jess
  [x]
  (cond (function-declaration? x) (function-declaration->jess x)
        (variable-declaration? x) (variable-declaration->jess x)
        :else
          (do
            (println x)
            (throw "invalid declaration"))))

(define-function expression->jess
  [x]
  (println x)
  (cond (this-expression? x) (this-expression->jess x)
        (array-expression? x) (array-expression->jess x)
        (object-expression? x) (object-expression->jess x)
        (function-expression? x) (function-expression->jess x)
        (unary-expression? x) (unary-expression->jess x)
        (update-expression? x) (update-expression->jess x)
        (binary-expression? x) (binary-expression->jess x)
        (assignment-expression? x) (assignment-expression->jess x)
        (logical-expression? x) (logical-expression->jess x)
        (member-expression? x) (member-expression->jess x)
        (conditional-expression? x) (conditional-expression->jess x)
        (call-expression? x) (call-expression->jess x)
        (new-expression? x) (new-expression->jess x)
        (identifier? x) (identifier->jess x)
        (regexp-literal? x) (regexp-literal->jess x)
        (literal? x) (literal->jess x)
        ;(sequence-expression? x) (sequence-expression->jess x)
        :else
          (do
            (println x)
            (throw "invalid expression"))))

(define-function function?
  [x]
  (and (.-params x) (.-body x)))

(define-function expression-statement?
  [x]
  (and (statement? x) (= "ExpressionStatement" (.-type x)) (.-expression x)))

(define-function expression-statement->jess
  [x]
  (expression->jess (.-expression x)))

(define-function block-statement?
  [x]
  (and (statement? x) (= "BlockStatement" (.-type x)) (.-body x)))

(define-function block-statement->list
  [x]
  (map statement->jess (.-body x)))

(define-function block-statement->jess
  [x]
  (cons 'do (block-statement->list x)))

(define-function empty-statement?
  [x]
  (and (statement? x) (= "EmptyStatement" (.-type x))))

(define-function debugger-statement?
  [x]
  (and (statement? x) (= "DebuggerStatement" (.-type x))))

(define-function with-statement?
  [x]
  (and (statement? x) (= "WithStatement" (.-type x)) (.-object x) (.-body x)))

(define-function return-statement?
  [x]
  (and (statement? x) (= "ReturnStatement" (.-type x))))

(define-function return-statement->jess
  [x]
  (if (.-argument x)
    (list 'return (expression->jess (.-argument x)))
    '(return)))

(define-function labeled-statement?
  [x]
  (and (statement? x) (= "LabeledStatement" (.-type x)) (.-body x)))

(define-function break-statement?
  [x]
  (and (statement? x) (= "BreakStatement" (.-type x))))

(define-function break-statement->jess
  [x]
  (if (.-argument x)
    (list 'break (expression->jess (.-argument x)))
    '(break)))

(define-function continue-statement?
  [x]
  (and (statement? x) (= "ContinueStatement" (.-type x))))

(define-function continue-statement->jess
  [x]
  (if (.-argument x)
    (list 'continue (expression->jess (.-argument x)))
    '(continue)))

(define-function if-statement?
  [x]
  (and (statement? x) (= "IfStatement" (.-type x)) (.-test x) (.-consequent x)))

(define-function if-statement->jess
  [x]
  (if (.-alternate x)
    (list 'ifelse (expression->jess (.-test x)) (statement->jess (.-consequent x)) (statement->jess (.-alternate x)))
    (list 'ifelse (expression->jess (.-test x)) (statement->jess (.-consequent x)))))

(define-function switch-statement?
  [x]
  (and (statement? x) (= "SwitchStatement" (.-type x)) (.-discriminat x) (.-cases x)))

(define-function switch-case?
  [x]
  (and (node? x) (= "SwitchCase" (.-type x)) (.-consequent x)))

(define-function throw-statement?
  [x]
  (and (statement? x) (= "ThrowStatement" (.-type x)) (.-argument x)))

(define-function throw-statement->jess
  [x]
  (list 'throw (expression->jess (.-argument x))))

(define-function try-statement?
  [x]
  (and (statement? x) (= "TryStatement" (.-type x)) (.-block x)))

(define-function catch-clause?
  [x]
  (and (node? x) (= "CatchClause" (.-type x)) (.-param x) (.-body x)))

(define-function while-statement?
  [x]
  (and (statement? x) (= "WhileStatement" (.-type x)) (.-test x) (.-body x)))

(define-function do-while-statement?
  [x]
  (and (statement? x) (= "DoWhileStatement" (.-type x)) (.-test x) (.-body x)))

(define-function for-statement?
  [x]
  (and (statement? x) (= "ForStatement" (.-type x)) (.-body x)))

(define-function for-in-statement?
  [x]
  (and (statement? x) (= "ForInStatement" (.-type x)) (.-left x) (.-right x) (.-body x)))

(define-function function-declaration?
  [x]
  (and (declaration? x) (function? x) (= "FunctionDeclaration" (.-type x)) (.-id x)))

(define-function variable-declaration?
  [x]
  (and (declaration? x) (= "VariableDeclaration" (.-type x)) (.-kind x) (.-declarations x)))

(define-function variable-declaration->jess
  [x]
  (list (symbol (.-kind x)) (into [] (mapcat variable-declarator->vector (.-declarations x)))))

(define-function variable-declarator?
  [x]
  (and (node? x) (= "VariableDeclarator" (.-type x)) (.-id x)))

(define-function variable-declarator->vector
  [x]
  [(.-id x) (.-init x)])

(define-function this-expression?
  [x]
  (and (expression? x) (= "ThisExpression" (.-type x))))

(define-function this-expression->jess
  [x]
  (symbol "this"))

(define-function array-expression?
  [x]
  (and (expression? x) (= "ArrayExpression" (.-type x)) (.-elements x)))

(define-function array-expression->jess
  [x]
  (into [] (.-elements x)))

(define-function object-expression?
  [x]
  (and (expression? x) (= "ObjectExpression" (.-type x)) (.-properties x)))

(define-function object-expression->jess
  [x]
  (into {} (map property->vector (.-properties x))))

(define-function property?
  [x]
  (and (= "Property" (.-type x)) (.-key x) (.-value x) (.-kind x)))

(define-function property->vector
  [x]
  (if (not= "init" (.-kind x))
    (throw "no support for get or set in object expressions"))
  [(.-key x) (.-value x)])

(define-function function-expression?
  [x]
  (and (expression? x) (function? x) (= "FunctionExpression" (.-type x))))

(define-function function-expression->jess
  [x]
  (if (.-id x)
    (cons 'function
          (cons (identifier->jess (.-id x))
                (cons (into [] (map pattern->jess (.-params x)))
                      (block-statement->list (.-body x)))))
    (cons 'function
          (cons (into [] (map pattern->jess (.-params x)))
                (block-statement->list (.-body x))))))

(define function-declaration->jess function-expression->jess)

(define *unary-operator* #{"-" "+" "!" "typeof" "void" "delete"})

(define-function unary-expression?
  [x]
  (and (expression? x) (= "UnaryExpression" (.-type x)) (.-operator x) (.-prefix x) (.-argument x)))

(define-function unary-expression->jess
  [x]
  (if-not (*unary-operator* (.-operator x))
    (throw (str "invalid unary operator, got: " (.-operator x) ", expected one of: " (join *unary-operator* ", "))))
  (list (symbol (.-operator x)) (expression->jess (.-argument x))))

(define *update-operator* #{"++" "--"})

(define-function update-expression?
  [x]
  (and (expression? x) (= "UpdateExpression" (.-type x)) (.-operator x) (.-prefix x) (.-argument x)))

(define-function update-expression->jess
  [x]
  (if-not (*update-operator* (.-operator x))
    (throw (str "invalid unary operator, got: " (.-operator x) ", expected one of: " (join *update-operator* ", "))))
  (list (symbol (.-operator x)) (expression->jess (.-argument x))))

(define *binary-operator* #{"==" "!=" "===" "!==" "<" "<=" ">" ">=" "<<" ">>" ">>>" "+" "-" "*" "/" "%" "|" "^" "&" "in" "instanceof"})

(define-function binary-expression?
  [x]
  (and (expression? x) (= "BinaryExpression" (.-type x)) (.-operator x) (.-right x) (.-left x)))

(define-function binary-expression->jess
  [x]
  (if-not (*binary-operator* (.-operator x))
    (throw (str "invalid unary operator, got: " (.-operator x) ", expected one of: " (join *binary-operator* ", "))))
  (list (symbol (.-operator x)) (expression->jess (.-left x)) (expression->jess (.-right x))))

(define *assignment-operator*
  {"="    'set!
   "+="   nil
   "-="   nil
   "*="   nil
   "/="   nil
   "%="   nil
   "<<="  nil
   ">>="  nil
   ">>>=" nil
   "|="   nil
   "^="   nil
   "&="   nil})

(define-function assignment-expression?
  [x]
  (and (expression? x) (= "AssignmentExpression" (.-type x)) (.-operator x) (.-right x) (.-left x)))

(define-function assignment-expression->jess
  [x]
  (let [op (*assignment-operator* (.-operator x))]
    (if-not op
      (throw "invalid unary operator, only '=' is supported"))
    (list (symbol op) (expression->jess (.-left x)) (expression->jess (.-right x)))))

(define *logical-operator* #{"||" "&&"})

(define-function logical-expression?
  [x]
  (and (expression? x) (= "LogicalExpression" (.-type x)) (.-operator x) (.-right x) (.-left x)))

(define-function logical-expression->jess
  [x]
  (if-not (*logical-operator* (.-operator x))
    (throw (str "invalid unary operator, got: " (.-operator x) ", expected one of: " (join *logical-operator* ", "))))
  (list (symbol (.-operator x)) (expression->jess (.-left x)) (expression->jess (.-right x))))

(define-function member-expression?
  [x]
  (and (expression? x) (= "MemberExpression" (.-type x)) (.-object x) (.-property x)))

(define-function member-expression->jess
  [x]
  (list '.- (expression->jess (.-object x)) (expression->jess (.-property x))))

(define-function conditional-expression?
  [x]
  (and (expression? x) (= "ConditionalExpression" (.-type x)) (.-test x) (.-alternate x) (.-consequent x)))

(define-function conditional-expression->jess
  [x]
  (list 'if (expression->jess (.-text x)) (expression->jess (.-alternate x)) (expression->jess (.-consequent x)))))

(define-function call-expression?
  [x]
  (and (expression? x) (= "CallExpression" (.-type x)) (.-callee x) (.-arguments x)))

(define-function call-expression->jess
  [x]
  (cons (expression->jess (.-callee x)) (map expression->jess (.-arguments x))))

(define-function new-expression?
  [x]
  (and (expression? x) (= "NewExpression" (.-type x)) (.-callee x) (.-arguments x)))

(define-function new-expression->jess
  [x]
  (cons 'new (cons (expression->jess (.-callee x)) (map identity (.-arguments x)))))

(define-function sequence-expression?
  [x]
  (and (expression? x) (= "SequenceExpression" (.-type x)) (.-expressions x)))

(define-function estree->jess [obj]
  (statement->jess obj))
