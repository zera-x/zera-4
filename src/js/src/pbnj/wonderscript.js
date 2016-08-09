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
    return _.isNull(exp) ||
           _.isUndefined(exp) ||
           _.isKeyword(exp) ||
           _.isBoolean(exp) ||
           _.isString(exp) ||
           _.isNumber(exp) ||
           _.isDate(exp) ||
           _.isRegExp(exp) ||
           _.isMap(exp) ||
           _.isVector(exp) ||
           _.isSet(exp);
  };

  var evalSelfEvaluating = _.identity;

  var makeTagPredicate = function(tag) {
    return function(exp) {
      return _.isList(exp) && _.isSymbol(_.first(exp)) && _.equals(_.first(exp), tag);
    };
  };

  var isQuoted = makeTagPredicate(_.symbol('quote'));

  var evalQuote = _.second;

  var isVariable = _.isSymbol;

  var evalVariable = function(exp, env) {
    var name = _.name(exp);
    var ns = _.namespace(exp);
    var val;
    if (ns === 'js') {
      val = ROOT_OBJECT[name];
    }
    else if (ns !== null) {
      val = getModule(_.symbol(ns))[name];
    }
    else {
      val = env.lookup(name).get(name);
    }
    return val;
  };

  var isDefinition = makeTagPredicate(_.symbol('define'));

  var evalDefinition = function(exp, env) {
    var rest = _.rest(exp);
    var ident = _.first(rest);
    var name = _.name(ident);
    var ns = _.namespace(ident);
    env.define(name);
    var value = ws.eval(_.second(rest), env);
    if (!env.parent || ns != null) {
      var mod = ns != null ? getModule(_.symbol(ns)) : pbnj.MODULE_SCOPE;
      mod[name] = value;
    }
    if (_.isFunction(value)) {
      value.$lang$ws$ident = ident;
      env.setIdent(name);
    }
    return env.define(name, value);
  };

  var isLambda = makeTagPredicate(_.symbol('lambda'));

  var arity = function(fn) {
    return fn.$lang$ws$arity || fn.length;
  };

  var invocableToFn = function(invocable, args) {
    var func = null;
    if (_.isKeyword(invocable) && _.isMap(_.first(args))) {
      func = function() { return _.get(arguments[0], invocable) };
    }
    else if (_.isKeyword(invocable) && _.isSet(_.first(args))) {
      func = function() { return _.hasKey(arguments[0], invocable) };
    }
    else if (_.isAssociative(invocable)) {
      func = function() { return _.get(invocable, arguments[0]) };
    }
    else if (_.isSet(invocable)) {
      func = function() { return _.hasKey(invocable, arguments[0]) };
    }
    else {
      throw new Error(_.str("'", invocable, "' is not invocable"));
    }
    return func;
  };

  var funcIdent = function(func) {
    return func.$lang$ws$ident || func.name || 'anonymous';
  };

  ws.apply = function(func, args) {
    if (isInvocable(func)) {
      var func = invocableToFn(func);
    }

    if (arguments.length !== 2) throw new Error(_.str('wrong number of arguments expected: 2 arguments, got: ', arguments.length));
    if (!_.isFunction(func)) throw new Error(_.str("'", func, "' is not a function"));

    return func.apply(null, args ? _.intoArray(args) : [])
  };

  var evalLambda = function(exp, env) {
    var rest = _.rest(exp);
    var names = _.first(rest);
    var body = _.second(rest);
    var scope = env.extend();
    var argCount = _.count(names);

    _.each(names, function(name) {
      var sname = _.str(name);
      if (sname[0] === '&') {
        argCount *= -1;
      }
    });

    var lambda = function() {
      var args = arguments;
      if (argCount < -1 && args.length < Math.abs(argCount) - 1) {
        throw new Error(_.str('wrong number of arguments expected at least: ', Math.abs(argCount) - 1, ' got: ', args.length));
      }
      else if (argCount > 0 && argCount !== args.length) {
        throw new Error(_.str('wrong number of arguments expected: ', argCount, ' got: ', args.length));
      }

      var i = 0;
      _.each(names, function(name) {
        var sname = _.str(name);
        if (sname[0] === '&') {
          scope.define(sname.replace(/^&/, ''), _.list.apply(null, [].slice.call(args, i)));
        }
        else {
          scope.define(sname, i < args.length ? args[i] : false);
        }
        i++;
      });

      return ws.eval(body, scope);
    };

    lambda.$lang$ws$arity = argCount;
    lambda.$lang$ws$code = exp;
    return lambda;
  };

  var isBlock = makeTagPredicate(_.symbol('do'));

  var evalBlock = function(exp, env) {
    var body = _.rest(exp);
    var scope = env.extend();
    var value = null;
    _.each(body, function(exp) {
      value = ws.eval(exp, scope)
    });
    scope.setIdent('do');
    return value;
  };

  var isInvocable = function(exp) {
    return _.isKeyword(exp) || _.isAssociative(exp) || _.isSet(exp);
  }

  var isApplication = function(exp) {
    return _.isFunction(exp) || _.isList(exp) && (isVariable(_.first(exp)) || isLambda(_.first(exp)) || isInvocable(_.first(exp)))
  };

  var evalApplication = function(exp, env) {
    var func = _.first(exp);
    var args = _.map(_.rest(exp), function(exp) { return ws.eval(exp, env) });

    if (isVariable(func)) {
      func = evalVariable(func, env); 
    }
    else if (isLambda(func)) {
      func = evalLambda(func, env);
    }

    if (_.isFunction(func) || isInvocable(func)) {
      return ws.apply(func, args);
    }
    else {
      throw new Error(_.str("'", func, "' is not a function"));
    }
  };

  var isCond = makeTagPredicate(_.symbol('cond'));

  var isFalsy = function(val) {
    return val === false || _.isNull(val) || _.isUndefined(val);
  };

  var evalCond = function(exp, env) {
    var rest = _.rest(exp);
    var pairs = _.pair(rest);
    for (var i = 0; i < _.count(pairs); i++) {
      var pred = _.nth(_.nth(pairs, i), 0);
      var cons = _.nth(_.nth(pairs, i), 1);
      if (_.equals(pred, _.keyword('else')) || !isFalsy(ws.eval(pred, env))) {
        return ws.eval(cons, env);
      }
    };
    return null;
  };

  var isMacro = makeTagPredicate(_.symbol('define-syntax'));

  var MACROS = {};
  var evalMacro = function(exp, env) {
    var rest = _.rest(exp);
    var name = _.first(rest);
    var args = _.second(rest);
    var body = _.rest(_.rest(rest));
    var lambda = _.cons(_.symbol('lambda'), _.cons(args, body));
    var currentScope = pbnj.MODULE_SCOPE;
    var mod = _.namespace(name) ? getModule(_.symbol(_.namespace(name))) : pbnj.MODULE_SCOPE;
    mod['@@MACROS@@'] = mod['@@MACROS@@'] || {}; 
    var fn = evalLambda(lambda, env);
    fn.$lang$ws$ident = name; // give it an ident for stacktrace
    mod['@@MACROS@@'][_.name(name)] = fn;
    return name;
  };

  var macroexpand = function(exp) {
    if (exp == null) return exp
    var macro = null;
    var name = _.isList(exp) ? _.first(exp) : exp;
    var ns = _.isSymbol(name) ? _.namespace(name) : null;
    if (ns === 'js') return exp;
    var macros = ns ? getModule(_.symbol(ns))['@@MACROS@@'] : pbnj.MODULE_SCOPE['@@MACROS@@'];
    if (macros == null) return exp;
    else if ((macro = macros[_.name(name)])) {
      return macroexpand(macro(exp));
    }
    return exp;
  };

  var isVariableIntrospection = makeTagPredicate(_.symbol('defined?'));

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

  var isAssignment = makeTagPredicate(_.symbol('set!'));

  var evalAssignment = function(exp, env) {
    var name = _.str(_.second(exp).value);
    var value = ws.eval(_.second(_.rest(exp)), env);
    env.lookup(name).set(name, value);
    return value;
  };

  var isThrownException = makeTagPredicate(_.symbol('throw'));

  var evalThrownException = function(exp, env) {
    var error = ws.eval(_.second(exp), env);
    throw new Error(wsError(error, env.stacktrace()));
  };

  var type = function(value) {
    var t = typeof value;
    if (value == null) return 'nil';
    else if (t === 'object' && _.isCollection(value)) {
      if (_.isList(value)) return 'list';
      else if (_.isMap(value)) return 'map';
      else if (_.isSet(value)) return 'set';
      else {
        return 'collection';
      }
    }
    else {
      return t;
    }
  };

  var globalEnv = pbnj.env();
  globalEnv.define('not', function(x) { return !x });
  globalEnv.define('identical?', function(a, b) { return a === b });
  globalEnv.define('equiv?', function(a, b) { return a == b });
  globalEnv.define('=', _.equals);
  globalEnv.define('>', function(a, b) { return a > b });
  globalEnv.define('<', function(a, b) { return a < b });
  globalEnv.define('<=', function(a, b) { return a <= b });
  globalEnv.define('>=', function(a, b) { return a >= b });
  globalEnv.define('array', function() { return Array.prototype.slice.call(arguments) });
  globalEnv.define('object', function(parent) { return Object.create(parent ? parent : null) });
  globalEnv.define('println', console.log.bind(console));
  globalEnv.define('macroexpand', macroexpand);
  globalEnv.define('arity', arity);
  globalEnv.define('init', function(ctr) {
    if (!_.isFunction(ctr)) throw new Error(_.str("'", ctr, "' is not an constructor"));
    return new (Function.prototype.bind.apply(ctr, _.rest(arguments)));
  });
  globalEnv.define('property', function(obj, prop) {
    if (obj == null) throw new Error('nil is not an object');
    return obj[_.str(prop).replace(/^:/, '')];
  });
  globalEnv.define('set-property!', function(obj, prop, value) {
    if (obj == null) throw new Error('nil is not an object');
    obj[_.str(prop).replace(/^:/, '')] = value;
    return obj;
  });
  globalEnv.define('send', function(obj, method) {
    if (arguments.length !== 2) throw new Error(_.str('wrong number of arguments expected: 2 arguments, got: ', arguments.length));
    return obj[_.str(method).replace(/^:/, '')].apply(obj, _.toArray(arguments).slice(2))
  });
  globalEnv.define('responds-to?', function(obj, method) {
    if (arguments.length !== 2) throw new Error(_.str('wrong number of arguments expected: 2 arguments, got: ', arguments.length));
    var val = obj[_.str(method).replace(/^:/, '')];
    return !isFalsy(val) && _.isFunction(val);
  });
  globalEnv.define('struct', function(args, name) {
    var attrs = _.map(_.rest(args), function(name) { return _.str(name).replace(/^:/, '') });
    var ctr = function(/** values */) {
      var obj = this;
      var prefix = !name ? '' : _.str(name).replace(/^:/, '').toLowerCase();
      var values = _.toArray(args);
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
  /*
  globalEnv.define('method', function(obj, name, def) {
    if (!_.isFunction(def)) throw new Error("invalid definition");
    if (_.isFunction(obj)) {
      // treat as a generic function
      var method = function(obj) {
        if (arguments.length !== def.length) {
          throw new Error(_.str('wrong number of arguments expected: ', def.length,' arguments, got: ', arguments.length));
        }
        this(obj)
      };
      obj.table = obj.table || _.hashMap();
      obj.table = _.assoc(obj.table, , def);
      return method.bind(obj);
    }
    else if (_.isObject(obj)) {
      // treat as a single dispatch obj
      obj[_.name(name)] = def;
      return def;
    }
    else {
      throw new Error(_.str("'", obj, "' is not a valid value"));
    }
  });*/
  globalEnv.define('class', function(args, methods, name) {
    var attrs = _.map(_.rest(args), function(name) { return _.str(name).replace(/^:/, '') });
    var ctr = function(/** values */) {
      var obj = this;
      var prefix = !name ? '' : _.str(name).replace(/^:/, '').toLowerCase();
      var values = _.toArray(args);
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
  var evalModuleName = function(name, root) {
    if (name.type === 'symbol') throw new Error('symbol expected');
    var path = _.str(name.value).split('/')[0].split('.');
    var mod = root || {};
    _.each(path, function(name) {
      if (!_.isObject(mod[name])) mod[name] = {};
      mod = mod[name];
    });
    return mod;
  };
  var defineModule = function(name) {
    return evalModuleName(name, ROOT_OBJECT);
  };
  var getModule = function(name) {
    if (!_.isSymbol(name)) throw new Error('symbol expected');
    var path = _.str(name).split('/')[0].split('.');
    var mod = ROOT_OBJECT;
    _.each(path, function(name) {
      if (!_.isObject(mod[name])) {
        throw new Error(_.str("module '", name, "' does not exist"));
      }
      mod = mod[name];
    });
    return mod;
  }
  globalEnv.define('module', defineModule);
  pbnj.MODULE_SCOPE = pbnj.core; // default scope

  var importModule = function(mod) {
    for (var fn in mod) {
      if (pbnj.core.hasOwnProperty(fn)) {
        globalEnv.define(fn, mod[fn]);
      }
    }
  }
  importModule(pbnj.core);
  
  ws.pprint = function(exp) {
    if (exp == null) return 'nil';
    else if (_.isBoolean(exp)) return exp == true ? 'true' : 'false';
    else if (isQuoted(exp)) return _.str("'", ws.pprint(_.second(exp)))
    else if (_.isNumber(exp) || _.isKeyword(exp) || _.isSymbol(exp) || _.isCollection(exp)) return _.str(exp);
    else if (exp.$lang$ws$code) return ws.pprint(exp.$lang$ws$code);
    else {
      return _.str(exp);
    }
  };
  globalEnv.define('pprint', ws.pprint);

  ws.eval = function(exp, env) {
    var env = env || globalEnv;
    var exp = macroexpand(exp);
    if (isSelfEvaluating(exp)) return evalSelfEvaluating(exp);
    else if (isVariable(exp)) return evalVariable(exp, env);
    else if (isQuoted(exp)) return evalQuote(exp);
    else if (isDefinition(exp)) return evalDefinition(exp, env);
    else if (isCond(exp)) return evalCond(exp, env);
    else if (isLambda(exp)) return evalLambda(exp, env);
    else if (isBlock(exp)) return evalBlock(exp, env);
    else if (isAssignment(exp)) return evalAssignment(exp, env);
    else if (isVariableIntrospection(exp)) return evalVariableIntrospection(exp, env);
    else if (isMacro(exp)) return evalMacro(exp, env);
    else if (isThrownException(exp)) return evalThrownException(exp, env);
    else if (isApplication(exp)) return evalApplication(exp, env);
    else {
      throw new Error(_.str("invalid expression: '", exp, "'"));
    }
  };
  globalEnv.define('eval', ws.eval);
  globalEnv.define('apply', ws.apply);

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

  var wsError = function(e, trace) {
    var wsStack = ''; //_.str(ws.pprint(exp), ' @ ', stream.source(), ':', stream.line(), ':', stream.column(), '\n');
    if (!_.isEmpty(trace)) {
    wsStack += _.reduce(
            _.map(trace,
              function(call) {
                return _.str(call[0], ' @ ', call[1], ':', call[2], ':', call[3]) }),
            function(s, x) {
              return _.str(s, "\n", x) });
    }
    return _.str(e.message ? e.message : e, ":\n", wsStack, e.stack ? _.str("\n", e.stack) : null);
  };

  ws.readStream = function(stream) {
    var env = globalEnv.setSource(stream.source());
    try {
      var value = null;
      while (!stream.eof()) {
        var exp = stream.peek();
        value = ws.eval(exp, env);
        if (_.isFunction(value) || isBlock(exp)) {
          //console.log(exp.toString());
          env.setLocation(stream.line(), stream.column());
          //console.log(env.stacktrace());
        }
        stream.next();
      }
      return value;
    }
    catch(e) {
      throw new Error(wsError(e, env.stacktrace()));
    }
  };
  globalEnv.define('read-stream', ws.readStream);

  ws.readString = function(str, input) {
    //console.log(str);
    var stream = pbnj.reader.readString(str, input);
    return ws.readStream(stream);
  };
  globalEnv.define('read-string', ws.readString);

  ws.readFile = function(file) {
    var stream = pbnj.reader.readFile(file);
    return ws.readStream(stream);
  };
  globalEnv.define('read-file', ws.readFile);

  if (module != void 0 && module.exports) {
    ws.readFile(_.str(__dirname, "/wonderscript/core.ws"))
  }
  else {
    ws.readFile("src/pbnj/wonderscript/core.ws")
  }

  ws.__LINE__ = 0;
  globalEnv.define('*line*', ws.__LINE__);
  globalEnv.define('*environment*', 'development');
  // TODO: detect browser?
  globalEnv.define('*platform*', module !== void 0 && module.exports ? 'nodejs' : 'javascript');
});
