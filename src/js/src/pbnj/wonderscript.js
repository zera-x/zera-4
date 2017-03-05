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

  var isSelfEvaluating = ws.isSelfEvaluating = function(exp) {
    return _.isNull(exp) ||
           _.isUndefined(exp) ||
           _.isKeyword(exp) ||
           _.isBoolean(exp) ||
           _.isString(exp) ||
           _.isNumber(exp) ||
           _.isDate(exp) ||
           _.isRegExp(exp);
  };

  var evalSelfEvaluating = _.identity;

  var isCollectionLiteral = ws.isCollectionLiteral = function(exp) {
    return _.isMap(exp) || _.isVector(exp) || _.isSet(exp);
  };

  var evalCollectionLiteral = function(exp, env) {
    if (_.isMap(exp)) {
      return mori.into(mori.hashMap(),
              mori.map(
                function(xs) { return mori.vector(ws.eval(mori.nth(xs, 0), env), ws.eval(mori.nth(xs, 1), env)) },
                exp));
    }
    else if (_.isVector(exp)) {
      return mori.into(mori.vector(),
              mori.map(
                function(x) { return ws.eval(x, env) },
                exp));
    }
    else if (_.isSet(exp)) {
      return mori.into(mori.set(),
              mori.map(
                function(x) { return ws.eval(x, env) },
                exp));
    }
    else {
      throw new Error(str('invalid form: ', exp));
    }
  };

  var makeTagPredicate = function(tag) {
    return function(exp) {
      return _.isList(exp) && _.isSymbol(_.first(exp)) && _.equals(_.first(exp), tag);
    };
  };

  var isQuoted = ws.isQuoted = makeTagPredicate(_.symbol('quote'));

  var evalQuote = _.second;

  var isVariable = ws.isVariable = _.isSymbol;

  var lookupVariable = function(sym, env) {
    if (_.isSymbol(sym)) {
      var name = _.name(sym);
      var ns = _.namespace(sym);
      if (ns === null) {
        // find local scope
        scope = env.lookup(name);
        if (scope === null) {
          if (pbnj.MODULE_SCOPE[name] == null) {
            if (pbnj.core[name] == null) {
              throw new Error(_.str("Undefined variable: '", sym, "'"));
            }
            return pbnj.core;
          }
          return pbnj.MODULE_SCOPE;
        }
        else {
          return scope;
        }
      }
      else {
        if (ns === 'js') {
          return ROOT_OBJECT;
        }
        else {
          var mod = getModule(_.symbol(ns));
          if (name === 'x') {
            console.log('mod: ', mod)
          }
          return mod;
        }
      }
    }
    throw new Error("variable should be a symbol");
  };

  var getVariable = function(env, name) {
    return _.isEnv(env) ? env.get(name) : env[name];
  };

  var evalVariable = function(exp, env) {
    var name = _.name(exp);
    var val = getVariable(lookupVariable(exp, env), name);
    //if (val) return val;
    //throw new Error(_.str("Undefined variable: '", exp, "'"));
    return val;
  };

  var isDefinition = ws.isDefinition = makeTagPredicate(_.symbol('define'));

  // TODO: add meta data, (see Clojure's Vars)
  var evalDefinition = function(exp, env) {
    var rest  = _.rest(exp);
    var ident = _.first(rest);
    var name  = _.name(ident);
    var ns    = _.namespace(ident);

    // define value in module scope
    var mod = ns === null ? pbnj.MODULE_SCOPE : getModule(_.symbol(ns));
    if (!mod) throw new Error(_.str("module ", ns ," is undefined"));
    if (!mod["@@SCOPE@@"]) {
      mod["@@SCOPE@@"] = globalEnv.extend();
    }
    mod["@@SCOPE@@"].define(name); // so definition can refer to itself
    var value = ws.eval(_.second(rest), env);
    if (_.isFunction(value)) {
      value.$lang$ws$ident = _.str(ident);
    }
    mod["@@SCOPE@@"].define(name, value);
    mod[name] = value;
    env.define(name, value);
    return value;
  };

  var isLocalDefinition = ws.isLocalDefinition = makeTagPredicate(_.symbol('define-'));

  // TODO: add meta data, (see Clojure's Vars)
  var evalLocalDefinition = function(exp, env) {
    var rest  = _.rest(exp);
    var ident = _.first(rest);
    var name  = _.name(ident);
    var ns    = _.namespace(ident);

    if (ns) throw new Error("local definition names should not have namespaces");

    env.define(name); // so definition can refer to itself
    var value = ws.eval(_.second(rest), env);
    if (_.isFunction(value)) {
      value.$lang$ws$ident = ident;
    }
    env.define(name, value);
    return value;
  };


  var isLambda = ws.isLambda = makeTagPredicate(_.symbol('lambda'));

  var arity = function(fn) {
    return fn.$lang$ws$arity || fn.length;
  };

  var invocableToFn = function(invocable, args) {
    var func = null;
    if (_.isKeyword(invocable) && _.isMap(_.first(args))) {
      func = function() { return _.get(arguments[0], invocable); };
    }
    else if (_.isKeyword(invocable) && _.isSet(_.first(args))) {
      func = function() { return _.hasKey(arguments[0], invocable); };
    }
    else if (_.isAssociative(invocable)) {
      func = function() { return _.get(invocable, arguments[0]); };
    }
    else if (_.isSet(invocable)) {
      func = function() { return _.hasKey(invocable, arguments[0]); };
    }
    else {
      throw new Error(_.str("'", invocable, "' is not invocable"));
    }
    return func;
  };

  var funcIdent = function(func) {
    return func.$lang$ws$ident || func.name || 'anonymous';
  };

  ws.apply = function(func_, args, obj) {
    var obj = obj ? obj : null;
    if (isInvocable(func_)) {
      var func = invocableToFn(func_);
    }
    else {
      var func = func_;
    }

    if (arguments.length !== 1 && arguments.length !== 2 && arguments.length !== 3) {
      throw new Error(_.str('wrong number of arguments expected: 1, 2 or 3 arguments, got: ', arguments.length));
    }
    if (!_.isFunction(func)) throw new Error(_.str("'", func_, "' is not a function"));

    return func.apply(obj, args ? _.intoArray(args) : []);
  };

  ws.applyMethod = function(obj, method, args) {
    if (arguments.length !== 2 && arguments.length !== 3) {
      throw new Error(_.str('wrong number of arguments expected: 2 or 3 arguments, got: ', arguments.length));
    }

    if (!_.isFunction(method)) throw new Error("method should be a function");

    return method.apply(obj, args ? _.intoArray(args) : []);
  };

  // (lambda [x] x)
  // (lambda ([x] x)
  //         ([x y] [x y]))
  var lambdaID = 0;
  var evalLambda = function(exp, env) {
    var rest = _.rest(exp);
    var names = _.first(rest);
    var argCount = 0, bodies = {};
    if (_.isVector(names)) {
      argCount = _.count(names);

      _.each(names, function(name) {
        var sname = _.str(name);
        if (sname[0] === '&') {
          argCount = (argCount - 1) * -1;
        }
      });

      bodies[argCount] = {arity: argCount, args: names, code: _.rest(rest)};
      var body = bodies[argCount].body;
    }
    else if (_.isList(names)) {
      _.each(rest, function(fn) {
        var args = _.first(fn);
        var body = _.rest(fn);
        var argCount = _.reduce(args, function(sum, arg) { return (_.str(arg)[0] === '&' ? (sum + 1) * -1 : sum + 1) }, 0);
        bodies[argCount] = {arity: argCount, args: args, code: body};
      });
      argCount = _.first(_.sort(_.map(_.pluck(bodies, 'arity'), Math.abs))) * -1;
    }
    else {
      throw new Error('malformed expression: the second element of a lambda expression should be an argument vector or the beginning of a list of lambda bodies');
    }

    var id = _.str('lambda-', lambdaID++);

    var lambda = function() {
      var scope = env.extend();

      var ident = lambda.$lang$ws$ident || id;
      scope.setIdent(ident);

      var args = arguments;
      if (argCount <= 0 && args.length < Math.abs(argCount)) {
        throw new Error(_.str('"', ident, '" wrong number of arguments expected at least: ', Math.abs(argCount), ' got: ', args.length));
      }
      else if (argCount > 0 && argCount !== args.length) {
        throw new Error(_.str('"', ident, '" wrong number of arguments expected: ', argCount, ' got: ', args.length));
      }
      var argc = args.length;
      var body = bodies[argc];
      if (body == null) {
        for (var i = (argc * -1); i <= 0; i++) {
          body = bodies[i];
          if (body != null) break;
        }
      }

      var i = 0;
      _.each(body.args, function(name) {
        var sname = _.str(name);
        if (sname[0] === '&') {
          var list = _.list.apply(null, [].slice.call(args, i))
          scope.define(sname.replace(/^&/, ''), list);
        }
        else {
          scope.define(sname, i < args.length ? args[i] : false);
        }
        i++;
      });

      var ret = null;
      _.each(body.code, function(exp) {
        ret = ws.eval(exp, scope);
      });
      return ret;
    };

    lambda.$lang$ws$id = id;
    lambda.$lang$ws$arity = argCount;
    lambda.$lang$ws$code = exp;
    lambda.toString = function() {
      return _.str('#<Lambda id: ', lambda.$lang$ws$id,
                   ' ident: ', ws.inspect(lambda.$lang$ws$ident),
                   ' arity: ', lambda.$lang$ws$arity,
                   ' code: ', ws.inspect(this.$lang$ws$code));
    };
    return lambda;
  };

  var isBlock = ws.isBlock = makeTagPredicate(_.symbol('do'));

  var evalBlock = function(exp, env) {
    var body = _.rest(exp);
    var scope = env.extend();
    scope.setIdent('do');
    var value = null;
    _.each(body, function(exp) {
      value = ws.eval(exp, scope);
    });
    return value;
  };

  var isInvocable = function(exp) {
    return _.isKeyword(exp) || _.isAssociative(exp) || _.isSet(exp);
  };

  var isApplication = ws.isApplication = function(exp) {
    return _.isList(exp);
  };

  var evalApplication = function(exp, env) {
    var func = _.first(exp);
    if (_.str(func) === 'reduce') {
      var args = _.map(_.rest(exp), function(exp) {
        var ret = ws.eval(exp, env);
        return ret;
      });
    }
    else {
      var args = _.map(_.rest(exp), function(exp) { return ws.eval(exp, env); });
    }

    if (isVariable(func)) {
      func = evalVariable(func, env); 
    }
    else if (isLambda(func)) {
      func = evalLambda(func, env);
    }
    else {
      func = ws.eval(func, env);
    }

    if (_.isFunction(func) || isInvocable(func)) {
      return ws.apply(func, args);
    }
    else {
      console.log(ws.inspect(exp));
      throw new Error(_.str("'", func, "' is not a function"));
    }
  };

  var isCond = ws.isCond = makeTagPredicate(_.symbol('cond'));

  var isFalsy = ws.isFalsy = function(val) {
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
    }
    return null;
  };

  var isMacro = makeTagPredicate(_.symbol('define-macro'));

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

  var macroexpand = ws.macroexpand = function(exp) {
    var name = _.first(exp);
    if (_.isList(exp) && _.isSymbol(name)) {
      var ns = _.namespace(name);
      var macros = (ns == null ? pbnj.MODULE_SCOPE : getModule(_.symbol(ns)))['@@MACROS@@'];
      var m = (macros && macros[_.name(name)]);
      var macro = m == null ? pbnj.core["@@MACROS@@"][name] : m;
      if (macro) {
        return macroexpand(macro.apply(macro, _.intoArray(_.rest(exp))));
      }
    }
    return exp;
  };

  var isVariableIntrospection = ws.isVariableIntrospection = makeTagPredicate(_.symbol('defined?'));

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

  var isAssignment = ws.isAssignment = makeTagPredicate(_.symbol('set!'));

  var evalAssignment = function(exp, env) {
    var name = _.second(exp);
    var scope = lookupVariable(name, env);
    var value = ws.eval(_.second(_.rest(exp)), env);
    var ns = _.namespace(name);
    if (_.isEnv(scope)) {
      scope.set(name, value);
    }
    else {
      scope[name] = value;
      env.set(name, value);
    }
    return value;
  };

  var isThrownException = ws.isThrownException = makeTagPredicate(_.symbol('throw'));

  var evalThrownException = function(exp, env) {
    var error = ws.eval(_.second(exp), env);
    throw new Error(wsError(error, env.stacktrace()));
  };

  var isPropertyAccessor = ws.isPropertyAccessor = makeTagPredicate(_.symbol('.-'));

  var evalPropertyAccessor = function(exp, env) {
    var obj = ws.eval(_.second(exp), env);
    var prop = _.second(_.rest(exp));
    return obj[prop];
  };

  var isClassInstantiation = ws.isClassInstantiation = makeTagPredicate(_.symbol('new'));

  var evalClassInstantiation = ws.evalClassInstantiation = function(exp, env) {
    var ctr = ws.eval(_.second(exp), env);
    var args = _.intoArray(_.map(_.rest(exp), function(arg) { return ws.eval(arg, env) }));
    return new (ctr.bind.apply(ctr, args));
  };

  var evalModuleName = function(name, root) {
    if (!_.isSymbol(name)) throw new Error('symbol expected');
    if (!root) throw new Error('root object is required');
    var path = _.str(name).split('/')[0].split('.');
    var mod = root;
    for (var i = 0; i < path.length; i++) {
      var name = path[i];
      if (!_.isObject(mod[name])) {
        mod[name] = {};
      }
      mod = mod[name];
    }
    return mod;
  };

  var defineModule = function(name) {
    return evalModuleName(name, ROOT_OBJECT);
  };

  var isModuleDefinition = ws.isModuleDefinition = makeTagPredicate(_.symbol('module'));

  var evalModuleDefinition = function(exp, env) {
    var name = _.second(exp); 
    var mod = defineModule(name);
    if (mod) {
      mod["@@SCOPE@@"] = env.extend().setIdent(_.str(name));
      mod["@@NAME@@"] = name;
      pbnj.MODULE_SCOPE = mod;
      globalEnv.define(name, mod);
    }
    else {
      throw new Error(_.str('There was an error defining module "', name, '"'));
    }
    return null;
  };

  var isModuleRequire = ws.isModuleRequire = makeTagPredicate(_.symbol('require'));

  var evalModuleRequire = function(exp, env) {
    var name = _.second(exp);
    if (_.isSymbol(name)) {
      var path = _.str("src/", _.str(name).split('.').join('/'), ".ws");
      ws.readFile(path, env.extend().setIdent('require'));
    }
    else {
      throw new Error("module name should be a symbol");
    }
    return null;
  };

  var getModule = ws.getModule = function(name) {
    if (!_.isSymbol(name)) throw new Error('symbol expected');
    var path = _.str(name).split('/')[0].split('.');
    var mod = ROOT_OBJECT;
    _.each(path, function(nm) {
      if (!_.isObject(mod[nm])) {
        mod[nm] = {};
      }
      mod = mod[nm];
    });
    return mod;
  };

  var importModule = function(mod) {
    for (var fn in mod) {
      if (pbnj.core.hasOwnProperty(fn)) {
        globalEnv.define(fn, mod[fn]);
      }
    }
  };

  pbnj.MODULE_SCOPE = pbnj.core; // default scope
  pbnj.core["@@NAME@@"] = _.symbol('pbnj.core');
  pbnj.core["@@MACROS@@"] = {};

  var globalEnv = pbnj.core['@@SCOPE@@'] = pbnj.env().setSource('src/pbnj/wonderscript.js');
  importModule(pbnj.core);
  
  globalEnv.define('*source*', null);
  globalEnv.define('not', function(x) { return !x; });
  globalEnv.define('identical?', function(a, b) { return a === b; });
  globalEnv.define('equiv?', function(a, b) { return a == b; });
  globalEnv.define('=', _.equals);
  globalEnv.define('>', function(a, b) { return a > b; });
  globalEnv.define('<', function(a, b) { return a < b; });
  globalEnv.define('<=', function(a, b) { return a <= b; });
  globalEnv.define('>=', function(a, b) { return a >= b; });
  globalEnv.define('mod', function(a, b) { return a % b; });
  globalEnv.define('array', function() { return Array.prototype.slice.call(arguments); });
  globalEnv.define('object', function(parent) { return Object.create(parent ? parent : null); });
  globalEnv.define('println', console.log.bind(console));
  globalEnv.define('macroexpand', macroexpand);
  globalEnv.define('arity', arity);
  
  ws.inspect = function(exp) {
    if (exp == null) {
      return 'nil';
    }
    else if (_.isBoolean(exp)) {
      return exp === true ? 'true' : 'false';
    }
    else if (isQuoted(exp)) {
      return _.str("'", ws.inspect(_.second(exp)));
    }
    else if (exp.$lang$ws$code) {
      return ws.inspect(exp.$lang$ws$code);
    }
    else {
      return _.str(exp);
    }
  };
  globalEnv.define('inspect', ws.inspect);

  ws.pprint = function(exp) {
    console.log(ws.inspect(exp));
  }
  globalEnv.define('pprint', ws.pprint);

  ws.eval = function(exp, env) {
    var env = env || globalEnv;
    var exp = macroexpand(exp);

    if (isSelfEvaluating(exp)) {
      return evalSelfEvaluating(exp);
    }
    else if (isCollectionLiteral(exp)) {
      return evalCollectionLiteral(exp, env);
    }
    else if (isVariable(exp)) {
      return evalVariable(exp, env);
    }
    else if (isQuoted(exp)) {
      return evalQuote(exp);
    }
    else if (isDefinition(exp)) {
      return evalDefinition(exp, env);
    }
    else if (isLocalDefinition(exp)) {
      return evalLocalDefinition(exp, env);
    }
    else if (isCond(exp)) {
      return evalCond(exp, env);
    }
    else if (isLambda(exp)) {
      return evalLambda(exp, env);
    }
    else if (isBlock(exp)) {
      return evalBlock(exp, env);
    }
    else if (isAssignment(exp)) {
      return evalAssignment(exp, env);
    }
    else if (isVariableIntrospection(exp)) {
      return evalVariableIntrospection(exp, env);
    }
    else if (isMacro(exp)) {
      return evalMacro(exp, env);
    }
    else if (isThrownException(exp)) {
      return evalThrownException(exp, env);
    }
    else if (isPropertyAccessor(exp)) {
      return evalPropertyAccessor(exp, env);
    }
    else if (isClassInstantiation(exp)) {
      return evalClassInstantiation(exp, env);
    }
    else if (isModuleDefinition(exp)) {
      return evalModuleDefinition(exp, env);
    }
    else if (isModuleRequire(exp)) {
      return evalModuleRequire(exp, env);
    }
    else if (isApplication(exp)) {
      return evalApplication(exp, env);
    }
    else {
      throw new Error(_.str("invalid expression: '", exp, "'"));
    }
  };
  globalEnv.define('eval', ws.eval);
  globalEnv.define('apply', ws.apply);
  globalEnv.define('apply-method', ws.applyMethod);

  var wsError = function(e, trace) {
    var wsStack = ''; //_.str(ws.pprint(exp), ' @ ', stream.source(), ':', stream.line(), ':', stream.column(), '\n');
    if (!_.isEmpty(trace)) {
      wsStack = fmtStacktrace(trace)
    }
    return _.str(e.message ? e.message : e, ":\n", wsStack, e.stack ? _.str("\n", e.stack) : null);
  };

  function fmtStacktrace(trace) {
    return _.map(trace, function(x) { return _.str(x[1], "@", x[0] || 'unknown', ":", x[2]) }).join('\n');
  }

  ws.readStream = function(stream, env) {
    var env = (env || globalEnv).extend().setSource(stream.source());
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
      console.log(e);
      console.log(fmtStacktrace(env.stacktrace()));
      //throw new Error(wsError(e, env.stacktrace()));
    }
  };
  globalEnv.define('read-stream', ws.readStream);

  ws.compileStream = function(stream) {
    var env = globalEnv.setSource(stream.source());
    try {
      var buffer = [];
      while (!stream.eof()) {
        var exp = stream.peek();
        buffer.push(ws.compile(exp));
        stream.next();
      }
      return buffer.join('\n');
    }
    catch(e) {
      throw new Error(wsError(e, env.stacktrace()));
    }
  };
  globalEnv.define('compile-stream', ws.compileStream);

  ws.readString = function(str, input) {
    //console.log(str);
    var stream = pbnj.reader.readString(str, input);
    return ws.readStream(stream);
  };
  globalEnv.define('read-string', ws.readString);

  ws.compileString = function(str, input) {
    var stream = pbnj.reader.readString(str, input);
    return ws.compileStream(stream);
  };
  globalEnv.define('compile-string', ws.compileString);

  ws.readFile = function(file, env) {
    var stream = pbnj.reader.readFile(file);
    return ws.readStream(stream, env);
  };
  globalEnv.define('read-file', ws.readFile);

  ws.compileFile = function(file) {
    var stream = pbnj.reader.readFile(file);
    return ws.compileStream(stream);
  };
  globalEnv.define('compile-file', ws.compileFile);

  if (module != void 0 && module.exports) {
    ws.readFile(_.str(__dirname, "/wonderscript/core.ws"));
  }
  else {
    //ws.readFile("src/pbnj/wonderscript/core.ws");
  }

  globalEnv.define('*environment*', 'development');
  // TODO: detect browser?
  globalEnv.define('*platform*', module !== void 0 && module.exports ? 'nodejs' : 'browser');
});
