goog.provide('pbnj.wonderscript');

goog.require('pbnj.core');
goog.require('pbnj.env');
goog.require('pbnj.reader');

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

  var isApplication = function(exp) {
    return _.isFunction(exp) || _.isList(exp) && (isVariable(_.first(exp)) || isLambda(_.first(exp)))
  };

  var evalApplication = function(exp, env) {
    var func = _.first(exp);
    var args = _.intoArray(_.map(_.rest(exp), function(exp) { return ws.eval(exp, env) }));
    if (isVariable(func)) {
      func = evalVariable(func, env); 
    }
    else if (isLambda(func)) {
      func = evalLambda(func, env);
    }

    if (_.isFunction(func)) {
      return func.apply(null, _.intoArray(args));
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
  globalEnv.define('println', console.log.bind(console));

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
    if (isSelfEvaluating(exp)) return exp;
    else if (isVariable(exp)) return evalVariable(exp, env);
    else if (isQuoted(exp)) return evalQuote(exp);
    else if (isDefinition(exp)) return evalDefinition(exp, env);
    else if (isCond(exp)) return evalCond(exp, env);
    else if (isLambda(exp)) return evalLambda(exp, env);
    else if (isApplication(exp)) return evalApplication(exp, env);
    else {
      throw new Error(_.str("invalid expression: '", exp, "'"));
    }
  };
  globalEnv.define('eval', ws.eval);
  globalEnv.define('apply', function(func, args) {
    if (arguments.length !== 2) throw new Error(_.str('wrong number of arguments expected: 2 arguments, got: ', arguments.length));
    if (!_.isFunction(func)) throw new Error(_.str("'", func, "' is not a function"));
    return func.apply(null, _.intoArray(args))
  });

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
