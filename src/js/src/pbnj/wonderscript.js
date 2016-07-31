goog.provide('pbnj.wonderscript');

goog.require('pbnj.core');
goog.require('pbnj.env');
goog.require('pbnj.reader');
goog.require('pbnj.emitter');

goog.scope(function() {
  var ws = pbnj.wonderscript;
  var _ = pbnj.core;

  if (module != void 0 && module.exports) {
    module.exports.wonderscript = pbnj.wonderscript;
  }

  var isSelfEvaluating = function(exp) {
    return _.isNull(exp) || _.isUndefined(exp) || _.isKeyword(exp) || _.isBoolean(exp) || _.isString(exp) || _.isNumber(exp) || _.isDate(exp) || _.isRegExp(exp) || _.isMap(exp) || _.isVector(exp) || _.isSet(exp);
  };

  var isQuoted = function(exp) {
    return _.isList(exp) && _.equals(_.first(exp), _.symbol('quote'));
  };

  var evalQuote = function(exp) {
    return _.second(exp);
  };

  var isVariable = _.isSymbol;

  var evalVariable = function(exp, env) {
    var name = _.str(exp);
    return env.lookup(name).get(name);
  };

  var isDefinition = function(exp) {
    return _.isList(exp) && _.equals(_.first(exp), _.symbol('def'));
  }

  var evalDefinition = function(exp, env) {
    var rest = _.rest(exp);
    var name = _.first(rest);
    var value = _.second(rest);
    return env.define(_.str(name), ws.eval(value, env));
  };

  var isLambda = function(exp) {
    return _.isList(exp) && _.equals(_.first(exp), _.symbol('lambda'));
  };

  var evalLambda = function(exp, env) {
    var rest = _.rest(exp);
    var names = _.first(rest);
    var body = _.rest(rest);
    var scope = env.extend();
    var argCount = _.count(names);
    return function lambda() {
      var args = arguments;
      if (argCount !== args.length) {
        throw new Error(_.str('wrong number of arguments expected: ', argCount, ' got: ', args.length));
      }
      var i = 0;
      _.each(names, function(name) {
        scope.define(name, i < args.length ? args[i] : false);
        i++;
      });
      var value = null;
      _.each(body, function(exp) {
        value = ws.eval(exp, scope)
      });
      return value;
    };
  };

  var isInvocable = function(exp) {
    return _.isKeyword(exp) || _.isAssociative(exp) || _.isSet(exp);
  }

  var isApplication = function(exp) {
    return _.isFunction(exp) || _.isList(exp) && (isVariable(_.first(exp)) || isLambda(_.first(exp)) || isInvocable(_.first(exp)))
  };

  var evalApplication = function(exp, env) {
    var func = _.first(exp);
    var args = _.intoArray(_.map(_.rest(exp), function(exp) { return ws.eval(exp, env) }));

    if (isVariable(func)) {
      try {
        func = evalVariable(func, env); 
      }
      catch (e) {
        if (_.isSymbol(func) && args[0][func.toString()]) {
          func = args[0][func.toString()].bind(args[0]);
          args = _.rest(args);
        }
        else {
          throw e;
        }
      }
    }
    else if (isLambda(func)) {
      func = evalLambda(func, env);
    }

    if (_.isFunction(func)) {
      return func.apply(null, _.intoArray(args));
    }
    else if (_.isKeyword(func) && _.isMap(args[0])) {
      return _.get(args[0], func);
    }
    else if (_.isKeyword(func) && _.isSet(args[0])) {
      return _.hasKey(args[0], func);
    }
    else if (_.isAssociative(func)) {
      return _.get(func, args[0]);
    }
    else if (_.isSet(func)) {
      return _.hasKey(func, args[0]);
    }
    else {
      throw new Error(_.str("'", func, "' is not a function"));
    }
  };

  var isCond = function(exp) {
    return _.isList(exp) && _.equals(_.first(exp), _.symbol('cond'));
  };

  var isFalsy = function(val) {
    return val === false || _.isNull(val) || _.isUndefined(val);
  };

  var evalCond = function(exp, env) {
    var rest = _.intoArray(_.rest(exp));
    var pairs = _.pair(rest);
    for (var i = 0; i < pairs.length; ++i) {
      var pred = pairs[i][0];
      var cons = pairs[i][1];
      if (_.equals(pred, _.keyword('else')) || !isFalsy(ws.eval(pred, env))) {
        return ws.eval(cons, env);
      }
    }
    return null;
  };

  var isMacro = function(exp) {
    return _.isList(exp) && _.equals(_.first(exp), _.symbol('defmacro'));
  };

  var MACROS = {};
  var evalMacro = function(exp, env) {
    var rest = _.rest(exp);
    var name = _.first(rest);
    var args = _.second(rest);
    var body = _.rest(_.rest(rest));
    var lambda = _.cons(_.symbol('lambda'), _.cons(args, body));
    console.log('lambda: ', _.str(lambda));
    MACROS[name.toString()] = evalLambda(lambda, env)
  };

  var macroexpand = function(exp) {
    var macro = null;
    if (_.isSymbol(exp) && (macro = MACROS[_.str(exp)])) {
      return macro(exp);
    }
    else if (_.isList(exp) && (macro = MACROS[_.str(_.first(exp))])) {
      return macro(exp);
    }
    return exp;
  };

  var isVariableIntrospection = function(exp) {
    return _.isList(exp) && _.equals(_.first(exp), _.symbol('defined?'));
  };

  var evalVariableIntrospection = function(exp, env) {
    var name = _.str(ws.eval(_.second(exp), env));
    try {
      env.lookup(name);
      return true;
    }
    catch (e) {
      return false;
    }
  };

  var isAssignment = function(exp) {
    return _.isList(exp) && _.equals(_.first(exp), _.symbol('set!'));
  };

  var evalAssignment = function(exp, env) {
    var name = _.str(_.second(exp));
    var value = ws.eval(_.second(_.rest(exp)), env);
    env.lookup(name).set(name, value);
    return value;
  };

  var globalEnv = pbnj.env();
  globalEnv.define('and', function(a, b) { return a && b });
  globalEnv.define('or', function(a, b) { return a || b });
  globalEnv.define('not', function(x) { return !x });
  globalEnv.define('identical?', function(a, b) { return a === b });
  globalEnv.define('equiv?', function(a, b) { return a == b });
  globalEnv.define('=', _.equals);
  globalEnv.define('>', function(a, b) { return a > b });
  globalEnv.define('<', function(a, b) { return a < b });
  globalEnv.define('<=', function(a, b) { return a <= b });
  globalEnv.define('>=', function(a, b) { return a >= b });
  globalEnv.define('array', function() { return Array.prototype.slice.call(arguments) });
  globalEnv.define('println', console.log.bind(console));
  globalEnv.define('macroexpand', macroexpand);
  globalEnv.define('send', function(obj, method) {
    if (arguments.length !== 2) throw new Error(_.str('wrong number of arguments expected: 2 arguments, got: ', arguments.length));
    return obj[_.str(method).replace(/^:/, '')].apply(obj, _.toArray(arguments).slice(2))
  });
  globalEnv.define('responds-to?', function(obj, method) {
    if (arguments.length !== 2) throw new Error(_.str('wrong number of arguments expected: 2 arguments, got: ', arguments.length));
    var val = obj[_.str(method).replace(/^:/, '')];
    return !isFalsy((val && _.isFunction(val)));
  });
  globalEnv.define('struct', function(name) {
    var attrs = _.map(_.rest(arguments), function(name) { return _.str(name).replace(/^:/, '') });
    var ctr = function(/** values */) {
      var obj = this;
      var prefix = _.str(name).replace(/^:/, '').toLowerCase();
      var values = _.toArray(arguments);
      _.each(values, function(value, i) {
        obj[_.str(prefix, '-', attrs[i])] = function() {
          return value;
        };
      });
    };
    return function() {
      return new (Function.prototype.bind.apply(ctr, _.cat({}, _.toArray(arguments))));
    }
  });

  var importModule = function(mod) {
    for (var fn in mod) {
      if (pbnj.core.hasOwnProperty(fn)) {
        globalEnv.define(fn, mod[fn]);
      }
    }
  }
  importModule(pbnj.core);
  
  ws.eval = function(exp, env) {
    var env = env || globalEnv;
    var exp = macroexpand(exp);
    if (isSelfEvaluating(exp)) return exp;
    else if (isVariable(exp)) return evalVariable(exp, env);
    else if (isQuoted(exp)) return evalQuote(exp);
    else if (isDefinition(exp)) return evalDefinition(exp, env);
    else if (isCond(exp)) return evalCond(exp, env);
    else if (isLambda(exp)) return evalLambda(exp, env);
    else if (isAssignment(exp)) return evalAssignment(exp, env);
    else if (isVariableIntrospection(exp)) return evalVariableIntrospection(exp, env);
    else if (isMacro(exp)) return evalMacro(exp, env);
    else if (isApplication(exp)) return evalApplication(exp, env);
    else {
      throw new Error(_.str("invalid expression: '", exp, "'"));
    }
  };
  globalEnv.define('eval', ws.eval);
  globalEnv.define('apply', function(func, args) {
    if (arguments.length !== 1) throw new Error(_.str('wrong number of arguments expected: 1 arguments, got: ', arguments.length));
    if (!_.isFunction(func)) throw new Error(_.str("'", func, "' is not a function"));
    return func.apply(null, args ? _.intoArray(args) : [])
  });

  ws.compile = function(exp, emitter) {
    var env = env || globalEnv;
    var emitter = emitter || pbnj.emitter.js;
    if (isSelfEvaluating(exp)) return emitter(ws.compile).selfEvaluating(exp);
    else if (isVariable(exp)) return emitter(ws.compile).variable(exp, env);
    else if (isQuoted(exp)) return emitter(ws.compile).quote(exp);
    else if (isDefinition(exp)) return emitter(ws.compile).definition(exp, env);
    else if (isCond(exp)) return emitter(ws.compile).cond(exp, env);
    else if (isLambda(exp)) return emitter(ws.compile).lambda(exp, env);
    else if (isApplication(exp)) return emitter(ws.compile).application(exp, env);
    else {
      throw new Error(_.str("invalid expression: '", exp, "'"));
    }
  };
  globalEnv.define('compile', ws.compile);

  ws.readString = function(str) {
    var value = null;
    _.each(pbnj.reader.readString(str), function(exp) {
      value = ws.eval(exp, globalEnv);
    });
    return value;
  };

  globalEnv.define('read-string', ws.readString);

  if (module != void 0 && module.exports) {
    var fs = require('fs');
    ws.readFile = function(file) {
      var buffer = fs.readFileSync(file);
      return ws.readString(buffer.toString());
    };
    globalEnv.define('read-file', ws.readFile);
  }
});
