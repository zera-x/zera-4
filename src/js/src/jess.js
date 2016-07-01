goog.provide('jess');

goog.scope(function() {
  
  // [Object] -> String
  var str = function () {
    if ( arguments.length === 0 ) return "";
    else if ( arguments.length === 1 ) return "" + arguments[0];
    else {
      return Array.prototype.slice.call(arguments).join('');
    }
  };

  // Array -> Array
  var pair = function (a) {
    var i, pairs = [], pair = [];
    for (i = 0; i < a.length; ++i) {
      if ( i % 2 === 0 ) {
        pair.push(a[i]);
      }
      else {
        pair.push(a[i]);
        pairs.push(pair);
        pair = []
      }
    }
    return pairs;
  };

  // Array -> Function -> Array
  var map = function (a, fn) {
    var newA = [], i;
    for (i = 0; i < a.length; ++i) {
      newA.push(fn.call(null, a[i], i));
    }
    return newA;
  };

  // Array -> String -> Array
  var pluck = function (a, prop) {
    return map(a, function (val) { return val[prop] });
  };

  // Object -> String -> JsValue -> Object
  var assoc = function (obj, key, value) {
    var newObj = {}, k;
    for (k in obj) {
      newObj[k] = obj[k];
    }
    newObj[key] = value;
    return newObj;
  };
  
  var pprint = function(form) {
    if ( typeof form === 'string' ) return str('"', form, '"');
    else if ( typeof form === 'boolean' ) return form;
    else if ( form === null ) return 'null';
    else if ( typeof form === 'undefined' ) return 'undefined';
    else if ( form instanceof Date ) return str('new Date(', form.valueOf(), ')');
    else if ( form instanceof RegExp ) {
      var flags = [];
      if ( form.ignoreCase ) flags.push('i');
      if ( form.global ) flags.push('g');
      if ( form.multiline ) flags.push('m');
      return str('/', form.source, '/', flags.join(''));
    }
    else if ( form instanceof Array ) {
      var delim = form.type === 'list' ? ['(', ')'] : ['[', ']']
        , sep = form.type === 'list' ? ' ' : ', ';
      return str(delim[0], map(form, function(x){ return pprint(x) }).join(sep), delim[1]);
    }
    else {
      return "" + form;
    }
  };

  jess.pprint = pprint;

  jess.eval = function (form) {
    return eval(jess.emit(form));
  };

  // JsValue -> String
  //
  // Sub-types:
  // String
  // Number
  // Boolean
  // Null
  // Undefined
  // Date
  // RegExp
  // Quote
  // Array
  // Object
  // SpecialForm
  //    IfElse
  //    If
  //    Exitential(?)
  //    InstancePred(instance?)
  jess.emit = function (form, opts) {
    var value, i, key;
    var env = env || {};
    var opts = opts || {};
    var pretty = opts.pretty || true;

    if ( typeof form === 'string' || form instanceof String ) {
      if ( /^".*"$/.test(form) ) {
        return JSON.stringify(form.replace(/^"/, '').replace(/"$/, ''));
      }
      else {
        // a symbol
        if ( env[form] ) return env[form];
        return form;
      }
    }
    else if ( typeof form === 'number' ) {
      return str('(', form.toString(), ')');
    }
    else if ( typeof form === 'boolean' ) {
      return str('(', form, ')');
    }
    else if ( form === null ) {
      return 'null';
    }
    else if ( typeof form === 'undefined' ) {
      return 'undefined';
    }
    else if ( form instanceof Date ) {
      return str('(new Date(', form.valueOf(), '))');
    }
    else if ( form instanceof RegExp ) {
      var flags = [];
      if ( form.ignoreCase ) flags.push('i');
      if ( form.global ) flags.push('g');
      if ( form.multiline ) flags.push('m');
      return str('(new RegExp("', form.source, '", "', flags.join(''), '"))');
    }
    else if ( form instanceof Array ) {
      if ( form[0] == null ) throw new Error(str("Invalid form: ", pprint(form)));
      switch(form[0].toString()) {
        case 'if-else':
          return jess.emitIfElse(form, env, opts);
        case 'if':
          return jess.emitIf(form, env, opts);
        case '?':
          return emitExistential(form, env, opts);
        case 'instance?':
          return str(jess.emit(form[1]), ' instanceof ', jess.emit(form[2]));
        case 'type':
          return str('typeof ', jess.emit(form[1]));
        case 'label':
          if ( form[1] == null ) throw new Error('a label cannot be null or undefined');
          return str(form[1], ':');
        case 'do':
          return jess.emitBlock(form.slice(1));
        case 'var':
        case 'const':
        case 'let':
          return jess.emitDef(form, env, opts);
        case 'function':
        case 'function*':
          return jess.emitFunction(form, env, opts);
        case 'return':
        case 'break':
        case 'continue':
        case 'throw':
        case 'delete':
          return jess.emitStatement(form);
        case 'case':
        case 'default':
          return jess.emitColonStatement(form);
        case 'catch':
        case 'while':
        case 'switch':
          return jess.emitControlFlow(form);
        case 'for':
          throw new Error("not implemented");
        case 'try':
          return jess.emitNamedBlock(form);
        // object property resolution
        case '.-':
          return jess.emitObjectRes(form);
        // object method call
        case '.':
          return jess.emitMethodCall(form);
        case 'new':
          return jess.emitClassInit(form);
        case '.-set!':
          if ( form.length !== 4 ) throw new SyntaxError("Object property assignment is malformed");
          return str(jess.emit(form[1]), '[', jess.emit(form[2]) , '] = ', jess.emit(form[3]));
        case 'set!':
          return jess.emitAssignment(form);
        // unary operators
        case '!':
        case 'not':
          return str('!(', jess.emit(form[1]), ')');
        case '--':
        case '++':
        case '~':
          return str(form[0], '(', jess.emit(form[1]), ')');
        // binary operators
        case '||':
        case 'or':
          return jess.emitBinOperator(['||'].concat(form.slice(1)));
        case '&&':
        case 'and':
          return jess.emitBinOperator(['&&'].concat(form.slice(1)));
        case '|':
        case '&':
        case '<<':
        case '>>':
        case '%':
        case '<':
        case '>':
        case '<=':
        case '>=':
        case '+':
        case '-':
        case '/':
        case '*':
        case '==':
        case '!=':
        case '===':
        case '!==':
          return jess.emitBinOperator(form);
        case 'quote':
          return JSON.stringify(form[1]);
        case 'comment':
          return null;
        default:
          // method resolution
          if ( /^\.\-?[\w_$]+/.test(form[0]) ) {
            var res = /^(\.\-?)([\w_$]+)/.exec(form[0]);
            var newForm = [res[1], form[1], res[2]].concat(form.slice(2));
            return jess.emit(newForm);
          }
          // class instantiation
          else if ( /^[\w_$]+\.$/.test(form[0]) ) {
            var res = /^([\w_$]+)\.$/.exec(form[0]);
            return jess.emit(['new', res[1]].concat(form.slice(1)));
          }
          // function application
          else {
            return jess.emitFuncApplication(form);
          }
      }
    }
    else if ( typeof form === 'object' && form.toSource ) {
      return form.toSource();
    }
    else if ( typeof form === 'object' ) {
      return jess.emitObj(form);
    }
    else {
      //return str('(', JSON.stringify(form), ')');
      throw new Error(str('invalid form: ', pprint(form)));
    }
  };

  jess.emitAssignment = function (form) {
    var name = form[1]
      , val = jess.emit(form[2]);
    return str(name, " = ", val);
  }

  jess.emitBinOperator = function (form) {
    var op = form[0]
      , values = form.slice(1, form.length)
      , valBuffer = [], i;
    for (i = 0; i < values.length; ++i) {
      valBuffer.push(jess.emit(values[i]));
    }
    return str('(', valBuffer.join(op), ')');
  }

  jess.emitDef = function (form, env, opts) {
    var name = form[0]
      , def  = form[1];

    if ( form.length === 2 ) {
      return str(name, " ", def, ";");
    }
    else if ( form.length > 2 ) {
      var defs = pair(form.slice(1));
      return str(name, ' ',
              map(
                defs,
                function (pair) {
                  return pair.join(' = ') }).join(', '), ';')
    }
    else {
      throw new Error("definition is malformed");
    }
  }

  jess.emitStatement = function (form) {
    if ( form.length === 1 ) {
      return str(form[0], ';');
    }
    else if ( form.length === 2 ) {
      return str(form[0], ' ', jess.emit(form[1]), ';');
    }
    else {
      throw new Error(str("statment is malformed: ", pprint(form)));
    }
  }

  jess.emitColonStatement = function (form) {
    if ( form.length === 1 ) {
      return str(form[0], ':');
    }
    else if ( form.length === 2 ) {
      return str(form[0], ' ', form[1], ':');
    }
    else {
      throw new Error(str("statment is malformed: ", pprint(form)));
    }
  }

  // NAME EXPR BLOCK
  // eg:
  // while () {}
  // for () {}
  jess.emitControlFlow = function (form) {
    var name = form[0]
      , expr = form[1]
      , block = form.slice(2);
    return str(name, ' (', jess.emit(expr), ') ', jess.emitBlock(block));
  };

  jess.emitBlock = function (exprs) {
    return str('{', map(exprs, jess.emit).join('; '), '}');
  };

  jess.emitIfElse = function (form, env, opts) {
    var exprs = pair(form.slice(1))
      , i
      , cond
      , buff = [];

    for (i = 0; i < exprs.length; ++i) {
      cond = i === 0 ? 'if' : 'else if';
      if ( exprs[i][0] === 'else' )
        buff.push(str('else { ', jess.emit(exprs[i][1]), ' }')); 
      else
        buff.push(str(cond, '(', jess.emit(exprs[i][0]), '){ ', jess.emit(exprs[i][1]), ' }')); 
    }

    return buff.join(' ');
  }

  jess.emitIf = function (form) {
    var pred = jess.emit(form[1])
      , cons = jess.emit(form[2])
      , alt;
    
    if ( typeof form[3] !== 'undefined' ) alt = jess.emit(form[3]);
  
    if ( !alt ) {
      return str("((", pred, ") ? ", cons, " : null)");
    }
    else {
      return str("((", pred, ") ? ", cons, " : ", alt, ")");
    }
  }

  jess.emitClassInit = function (form) {
    var args = map(form.slice(2), jess.emit).join(', ');
    return str('new ', jess.emit(form[1]), '(', args, ')');
  }

  // cases:
  // function () {}
  // function NAME () {}
  jess.emitFunction = function (form) {
    var i, name, args, body;
    
    // named function
    if ( form.length > 3 && typeof form[1] === 'string' ) {
      name = form[1];
      args = form[2];
      body = form.slice(3);
    }
    // anonymous function
    else if ( form.length >= 3 ) {
      args = form[1];
      body = form.slice(2);
    }
    // error
    else {
      throw new Error("'function' form requires 2 or 3 arguments");
    }

    // evaluate body
    var buf = [], code;
    for (i = 0; i < body.length; ++i) {
      buf.push(jess.emit(body[i]));
    }

    code = jess.emitBlock(body);

    if ( name ) {
      return str(form[0], ' ', name, ' (', args.join(','), ') ', code, '');
    }
    else {
      return str('(', form[0], ' (', args.join(','), ') ', code, ')');
    }
  }
  
  jess.emitObj = function (form) {
    var k, buf = [];
    for ( k in form ) {
      buf.push(str(k, ':', jess.emit(form[k])));
    }
    return str('({', buf.join(','), '})');
  };
  
  jess.emitExistential = function (form, env, opts) {
    var val = emit(form[1], env, opts);
    return str("(", emit(val, env), " != null)");
  }

  jess.emitNullTest = function (form, env, opts) {
    var val = emit(form[1], env, opts);
    return str("(", symbolize(val), " === null)");
  }
  
  function parseArgs(args) {
    var splat = false, name, argsBuf = [];
    for (var i = 0; i < args.length; ++i) {
      if ( /^&/.test(args[i]) ) {
        name = args[i].replace(/^&/, '')
        splat = true;
      }
      else {
        name = args[i];
      }
      argsBuf.push({name: name, order: i, splat: splat});
    }
    return argsBuf;
  }

  function genArgAssigns(argsBuf) {
    var argsAssign = [], i;
    for (i = 0; i < argsBuf.length; ++i) {
      if ( argsBuf[i].splat )
        argsAssign.push(str(argsBuf[i].name, " = Array.prototype.slice.call(arguments, ", i, ");"));
      else
        argsAssign.push(str(argsBuf[i].name, " = arguments[", argsBuf[i].order, "];"));
    }
    return argsAssign.join('');
  }

  function genArgsDef(argsBuf) {
    var argsDef = [];
    for (var i = 0; i < argsBuf.length; ++i) {
      argsDef.push(argsBuf[i].name);
    }
    return argsDef.join(',');
  }

  jess.emitObjectRes = function (form) {
    var obj = form[1]
      , prop = form[2]
    return str('(', jess.emit(obj), ')["', prop, '"]');
  }

  jess.emitMethodCall = function (form) {
    return str(jess.emitObjectRes(form), '(', map(form.slice(3), jess.emit).join(', '), ')');
  }

  jess.emitFuncApplication = function (form) {
    var fn = jess.emit(form[0])
      , args = form.slice(1, form.length)
      , argBuffer = [], i, value;
  
    for (i = 0; i < args.length; ++i) {
      value = jess.emit(args[i]);
      argBuffer.push(value);
    }
  
    if ( argBuffer.length === 0 ) {
      return str('(', fn, ').apply(this)');
    }
    return str('(', fn, ').apply(this, [', argBuffer.join(', ') ,"])");
  }
  
});
