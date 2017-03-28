namespace pbnj.wonderscript {
  var ws = wonderscript;

  var _, ROOT_OBJECT;
  if (typeof exports !== 'undefined') {
    mori = require('mori');
    _    = require('./core.js');
    var pbnjEnv = require('./env.js');
    var pbnj = pbnj || {};
    pbnj.env = pbnjEnv.env;
    pbnj.core = _;
    pbnj.core.isEnv = pbnjEnv.isEnv;
    pbnj.reader = require('./reader.js');
    ROOT_OBJECT = global;
  }
  else {
    ROOT_OBJECT = window;
    _ = ROOT_OBJECT.pbnj.core;
  }
  ROOT_OBJECT.pbnj = ROOT_OBJECT.pbnj || {};
  ROOT_OBJECT.pbnj.wonderscript = ws;
  ROOT_OBJECT.pbnj.core = _;
  ROOT_OBJECT.pbnj.reader = pbnj.reader;
  ROOT_OBJECT.pbnj.env = pbnj.env;

  var isSelfEvaluating = ws.isSelfEvaluating = function(exp) {
    return _.isNull(exp) ||
           _.isUndefined(exp) ||
           _.isBoolean(exp) ||
           _.isString(exp) ||
           _.isNumber(exp) ||
           _.isDate(exp) ||
           _.isRegExp(exp);
  };

  var makeTagPredicate = _.makeTagPredicate;

  var evalSelfEvaluating = _.identity;

  var evalKeyword = function(exp, env) {
    var sexp = _.str(exp);
    if (sexp.startsWith('::')) {
      var modname = env.lookup('*module-name*')
                        ? env.lookup('*module-name*').get('*module-name*')
                        : ws.MODULE_SCOPE["@@NAME@@"];
      return _.keyword(_.str(modname), sexp.replace(/^::/, ''));
    }
    return exp;
  };

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

  var isQuoted = ws.isQuoted = makeTagPredicate(_.symbol('quote'));

  var evalQuote = _.second;

  var isVariable = ws.isVariable = _.isSymbol;

  var lookupVariable = function(sym, env) {
    var env = env || globalEnv;
    if (_.isSymbol(sym)) {
      var name = _.name(sym);
      var ns = _.namespace(sym);
      if (ns === null) {
        // find local scope
        scope = env.lookup(name);
        if (scope === null) {
          if (ws.MODULE_SCOPE[name] == null) {
            if (pbnj.core[name] == null) {
              return null;
            }
            return pbnj.core;
          }
          return ws.MODULE_SCOPE;
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
          var mod = findModule(_.symbol(ns));
          if (mod == null) {
            return lookupVariable(_.symbol(ns));
          }
          return mod;
        }
      }
    }
    throw new Error("variable should be a symbol");
  };
  ws.lookupVariable = lookupVariable;

  var getVariable = function(env, name) {
    return _.isEnv(env) ? env.get(name) : env[name];
  };

  var evalVariable = function(exp, env) {
    var name = _.name(exp);
    var mod = lookupVariable(exp, env);
    if (mod === null) throw new Error(_.str("Undefined variable: '", exp, "'"));
    var val = getVariable(mod, name);
    return val;
  };

  var isDefinition = ws.isDefinition = makeTagPredicate(_.symbol('define'));

  var defineVariable = function(env, ident, value, meta) {
    var meta = meta || _.hashMap();
    var isPrivate = _.get(meta, _.keyword('private'), false);
    var name = _.name(ident);
    var ns   = _.namespace(ident);
    if (isPrivate) {
      env.define(name, value, _.assoc(meta, _.keyword('private'), true));
    }
    else {
      // define value in module scope
      var mod = ns == null ? pbnj.MODULE_SCOPE : getModule(_.symbol(ns));
      if (!mod) throw new Error(_.str("module ", ns ," is undefined"));
      if (!mod["@@SCOPE@@"]) {
        mod["@@SCOPE@@"] = globalEnv.extend();
      }
      meta = _.assoc(meta, _.keyword('module'), mod["@@NAME@@"]);
      mod["@@SCOPE@@"].define(name, value, meta);
      mod[name] = value;
    }
    return env;
  };
  ws.defineVariable = defineVariable;

  var evalDefinition = function(exp, env) {
    var rest  = _.rest(exp);
    var first = _.first(rest);
    if (_.isSymbol(first)) {
      var meta   = _.hashMap();
      var ident  = first;
      var preVal = _.second(rest);
    }
    else if (_.isMap(first)) {
      var meta   = first;
      var ident  = _.first(_.rest(rest));
      var preVal = _.second(_.rest(rest));
    }
    else if (_.isKeyword(first)) {
      var meta   = _.hashMap(first, true);
      var ident  = _.first(_.rest(rest));
      var preVal = _.second(_.rest(rest));
    }
    else {
      throw new Error('define: first element should be a symbol, keyword, or map');
    }

    var name = _.name(ident);
    var ns   = _.namespace(ident);
    
    env.define(name); // do definition can refer to itself

    var value = preVal == null ? null : ws.eval(preVal, env);
    defineVariable(env, ident, value, meta);

    if (_.isFunction(value)) {
      value.$lang$ws$ident = _.str(ident);
    }
    return value;
  };

  var isLambda = ws.isLambda = makeTagPredicate(_.symbol('lambda'));

  var arity = function(fn) {
    return fn.$lang$ws$arity || fn.length;
  };

  var invocableToFn = function(invocable, args) {
    var func = null;
    if (_.isKeyword(invocable) && _.isMap(_.first(args))) {
      func = function(map) { return _.get(map, invocable); };
    }
    else if (_.isKeyword(invocable) && _.isSet(_.first(args))) {
      func = function(set) { return _.hasKey(set, invocable); };
    }
    else if (_.isAssociative(invocable)) {
      func = function(key) { return _.get(invocable, key); };
    }
    else if (_.isSet(invocable)) {
      func = function(elem) { return _.hasKey(invocable, elem); };
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
      var func = invocableToFn(func_, args);
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
        throw new Error(_.str('"', ws.inspect(lambda), '" wrong number of arguments expected at least: ', Math.abs(argCount), ' got: ', args.length));
      }
      else if (argCount > 0 && argCount !== args.length) {
        throw new Error(_.str('"', ws.inspect(lambda), '" wrong number of arguments expected: ', argCount, ' got: ', args.length));
      }
      var argc = args.length;
      var body = bodies[argc];
      if (body == null) {
        for (var i = (argc * -1); i <= 0; i++) {
          body = bodies[i];
          if (body != null) break;
        }
        if (body == null) throw new Error(_.str("wrong number of arguments for: ", ws.inspect(lambda), ' got: ', args.length));
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

    lambda.$lang$ws$tag = _.keyword('lambda');
    lambda.$lang$ws$id = id;
    lambda.$lang$ws$arity = argCount;
    lambda.$lang$ws$code = exp;
    lambda.toString = function() {
      return _.str('#<Lambda id: ', lambda.$lang$ws$id,
                   ' ident: ', ws.inspect(lambda.$lang$ws$ident),
                   ' arity: ', lambda.$lang$ws$arity,
                   ' code: ', ws.inspect(this.$lang$ws$code));
    };
    //console.log(lambda);
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

  var isLoop = ws.isLoop = makeTagPredicate(_.symbol('loop'));
  var isRecursionPoint = ws.isRecursionPoint = makeTagPredicate(_.symbol('again'));

  var evalLoop = function(exp, env) {
    evalLoop.values = [];
    var bindings = _.second(exp);
    var body = _.rest(_.rest(exp));
    if (!_.isVector(bindings)) throw new Error('bindings should be a vector');
    var scope = env.extend().setIdent('loop');
    var names = [];
    for (var i = 0; i < _.count(bindings); i += 2) {
      var nm  = _.nth(bindings, i);
      scope.define(nm);
      var val = ws.eval(_.nth(bindings, i + 1), scope);
      names.push(nm);
      evalLoop.values.push(val);
    }
    var notonce = true;
    var recur = false;
    while (notonce || recur) {
      var exprs = body;
      if (recur) recur = false;
      for (var i = 0; i < names.length; i++) {
        scope.define(names[i], evalLoop.values[i]);
      }
      var value = null, exp = null;
      while (exp = _.first(exprs)) {
        value = ws.eval(exp, scope);
        if (_.equals(value, _.keyword('again'))) {
          recur = true;
        }
        exprs = _.rest(exprs);
      }
      notonce = false;
    }
    return value;
  };

  var evalRecursionPoint = function(exp, env) {
    var args = _.rest(exp);
    evalLoop.values = _.intoArray(_.map(args, function(x) { return ws.eval(x, env); }));
    return _.keyword('again');
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
      throw new Error(_.str("'", ws.inspect(exp), "' is not a function"));
    }
  };

  var isCond = ws.isCond = makeTagPredicate(_.symbol('cond'));

  var evalCond = function(exp, env) {
    var rest = _.rest(exp);
    if (_.count(rest) % 2 !== 0) throw new Error(_.str('cond requires an even number of elements ', ws.inspect(exp)));
    var pairs = _.pair(rest);
    for (var i = 0; i < _.count(pairs); i++) {
      var pred = _.nth(_.nth(pairs, i), 0);
      var cons = _.nth(_.nth(pairs, i), 1);
      if (_.equals(pred, _.keyword('else')) || !_.isFalse(ws.eval(pred, env))) {
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
    var ns = _.namespace(name);
    var mod = ns ? getModule(_.symbol(ns)) : ws.MODULE_SCOPE;
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
      if (ns === 'js') return exp;
      var mod = (ns == null ? ws.MODULE_SCOPE : findModule(_.symbol(ns)))
      if (mod == null) {
        var scope = lookupVariable(_.symbol(ns))
        if (scope === null) return exp;
        mod = getVariable(scope, ns);
        if (mod == null) {
          return exp;
        }
      }
      var macros = mod['@@MACROS@@'];
      var m = (macros && macros[_.name(name)]);
      var macro = m == null ? pbnj.core["@@MACROS@@"][name] : m;
      if (macro) {
        return macroexpand(macro.apply(macro, _.intoArray(_.rest(exp))));
      }
    }
    return exp;
  };

  var isAssignment = ws.isAssignment = makeTagPredicate(_.symbol('set!'));

  var evalAssignment = function(exp, env) {
    var name = _.second(exp);
    var scope = lookupVariable(name, env);
    if (scope === null) throw new Error(_.str("Undefined variable '", name, "'"));
    var value = ws.eval(_.second(_.rest(exp)), env);
    var ns = _.namespace(name);
    var sname = _.str(name);
    /*if (ns == null) {
      if (pbnj.MODULE_SCOPE[sname] != null) {
        pbnj.MODULE_SCOPE[sname] = value;
      }
    }*/
    if (_.isEnv(scope)) {
      scope.set(sname, value);
    }
    else {
      scope[sname] = value;
      if (env = env.lookup(sname)) {
        env.set(sname, value);
      }
    }
    return value;
  };

  var isThrownException = ws.isThrownException = makeTagPredicate(_.symbol('throw'));

  var evalThrownException = function(exp, env) {
    var error = ws.eval(_.second(exp), env);
    if (_.isString(error)) {
      throw new Error(wsError(error, env.stacktrace()));
    }
    else {
      throw error;
    }
  };

  var isPropertyAccessor = ws.isPropertyAccessor = makeTagPredicate(_.symbol('.-'));

  var evalPropertyAccessor = function(exp, env) {
    var obj = ws.eval(_.second(exp), env);
    if (obj == null) throw new Error(_.str("nil is not an object from: ", ws.inspect(_.second(exp))));
    var prop = _.second(_.rest(exp));
    if (_.isSymbol(prop)) {
      return obj[_.str(prop)];
    }
    else {
      return obj[ws.eval(prop, env)];
    }
  };

  var isPropertyAssignment = ws.isPropertyAssignment = makeTagPredicate(_.symbol('.-set!'));

  var evalPropertyAssignment = function(exp, env) {
    var obj = ws.eval(_.second(exp), env);
    if (obj == null) throw new Error("nil is not an object");
    var prop = _.second(_.rest(exp));
    var value = ws.eval(_.second(_.rest(_.rest(exp))), env);
    if (_.isSymbol(prop)) {
      return obj[_.str(prop)] = value;
    }
    else {
      return obj[ws.eval(prop, env)] = value;
    }
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
    var mod = evalModuleName(name, ROOT_OBJECT);
    if (mod) {
      var scope = globalEnv.extend().setIdent(_.str(name));
      scope.define('*module-name*', name);
      mod["@@SCOPE@@"] = scope;
      mod["@@NAME@@"] = name;
      mod.$lang$ws$type = 'module';
      pbnj.MODULE_SCOPE = mod;
      globalEnv.define(name, mod, _.hashMap(_.keyword('tag'), _.keyword('module')));
    }
    else {
      throw new Error(_.str('There was an error defining module "', name, '"'));
    }
    return mod;
  };
  ws.defineModule = defineModule;

  var isModuleDefinition = ws.isModuleDefinition = makeTagPredicate(_.symbol('module'));

  var evalModuleDefinition = function(exp, env) {
    var name = _.second(exp); 
    return defineModule(name);
  };

  var isModuleDefinition = ws.isModuleDefinition = makeTagPredicate(_.symbol('module'));

  var evalModuleDefinition = function(exp, env) {
    var name = _.second(exp); 
    var mod = defineModule(name);
    if (mod) {
      var scope = env.extend().setIdent(_.str(name));
      scope.define('*module-name*', name);
      mod["@@SCOPE@@"] = scope;
      mod["@@NAME@@"] = name;
      mod.$lang$ws$type = 'module';
      ws.MODULE_SCOPE = mod;
      globalEnv.define(name, mod);
    }
    else {
      throw new Error(_.str('There was an error defining module "', name, '"'));
    }
    return mod;
  };

  var isModuleRequire = ws.isModuleRequire = makeTagPredicate(_.symbol('require'));

  var evalModuleRequire = function(exp, env) {
    var name = _.second(exp);
    var mod = ws.MODULE_SCOPE;
    var dir = typeof exports !== 'undefined' ? env.lookup('*dir*').get('*dir*') : '/';

    if (_.isString(name)) {
      var full = dir ? [dir, name].join('/') : name;
      try {
        ws.readFile(full, env.extend().setIdent('require'));
      }
      catch (e) {
        throw new Error(_.str("Reading file: \"", full, "\": ", e.message ? e.message : e));
      }
    }
    else if (_.isSymbol(name)) {
      var path = _.str("src/", _.str(name).split('.').join('/'), ".ws");
      ws.readFile(dir ? [dir, path].join('/') : path, env.extend().setIdent('require'));
      var mod = findModule(name);
      if (mod == null) {
        throw new Error(_.str('module ', name, ' does not exist'));
      }
    }
    else {
      throw new Error("module name should be a symbol or string");
    }
    ws.MODULE_SCOPE = mod;
    return true;
  };

  var isModuleSet = ws.isModuleSet = makeTagPredicate(_.symbol('use'));

  var evalModuleSet = function(exp, env) {
    var name = _.second(exp);
    if (_.isSymbol(name)) {
      ws.MODULE_SCOPE = getModule(name);
      env.define('*module-name*', name);
    }
    else {
      throw new Error("module name should be a symbol");
    }
    return null;
  };

  var findModule = ws.getModule = function(name) {
    if (!_.isSymbol(name)) throw new Error('symbol expected');
    var path = _.str(name).split('/')[0].split('.');
    var mod = ROOT_OBJECT;
    for (var i = 0; i < path.length; i++) {
      mod = mod[path[i]];
      if (mod == null) break; //throw new Error(_.str('module ', name, ' is undefined'));
    }
    return mod;
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

  var isMethodApplication = ws.isMethodApplication = makeTagPredicate(_.symbol('.'));

  var evalMethodApplication = function(exp, env) {
    var args = _.rest(exp);
    var obj = ws.eval(_.first(args), env);
    if (obj == null) throw new Error(_.str('nil is not an object from: ', ws.inspect(exp)));
    var method = _.second(args);
    if (_.isList(method)) {
      var mname = _.first(method);
      var m = obj[_.str(mname)]
      if (m == null) {
        throw new Error(_.str('method "', ws.inspect(mname), '" does not exist'));
      }
      return m.apply(obj, _.intoArray(_.map(_.rest(method), function(x) { return ws.eval(x, env) })));
    }
    else {
      var m = obj[_.str(method)]
      if (m == null) {
        throw new Error(_.str('method "', ws.inspect(method), '" does not exist'));
      }
      return m.apply(obj);
    }
  };

  var isTryBlock = ws.isTryBlock = makeTagPredicate(_.symbol('try'));
  var isCatchBlock = ws.isCatchBlock = makeTagPredicate(_.symbol('catch'));
  var isFinallyBlock = ws.isFinallyBlock = makeTagPredicate(_.symbol('finally'));

  var evalCatchBlock = function(exp, e, env) {
    var binds = _.second(exp);
    var body = _.rest(_.rest(exp));
    var expr, value = null;
    var scope = env.extend().setIdent('catch');

    if (!_.isVector(binds) || _.count(binds) !== 2) {
      console.log('binds', ws.inspect(binds));
      throw new Error("bindings should be a vector of 2 elements");
    }

    var vari = _.first(binds);
    var klass = ws.eval(_.second(binds), scope);

    if (e instanceof klass) {
      scope.define(_.str(vari), e);
      while (expr = _.first(body)) {
        value = ws.eval(expr, scope);
        body = _.rest(body);
      }
    }
    return value;
  };

  var evalFinallyBlock = function(exp, env) {
    var body = _.rest(exp), expr, value;
    while (expr = _.first(body)) {
      value = ws.eval(expr, env);
      body = _.rest(body);
    }
    return value;
  };

  var evalTryBlock = function(exp, env) {
    var body = _.rest(exp), expr, value;
    var catchBlock = _.filter(body, isCatchBlock);
    var finallyBlock = _.first(_.filter(body, isFinallyBlock));
    body = _.reject(body, function(exp) { return isFinallyBlock(exp) || isCatchBlock(exp) });
    var scope = env.extend().setIdent('try');
    if (!catchBlock && !finallyBlock) throw new Error("A try block should have a catch block or a finally block");
    if (catchBlock) {
      try {
        while (expr = _.first(body)) {
          value = ws.eval(expr, scope);
          body = _.rest(body);
        }
      }
      catch (e) {
        var cBlock;
        while (cBlock = _.first(catchBlock)) {
          ws.pprint(cBlock);
          value = evalCatchBlock(cBlock, e, scope);
          catchBlock = _.rest(catchBlock);
        }
      }
    }
    if (finallyBlock) {
      value = evalFinallyBlock(finallyBlock, scope);
    }
    return value;
  };

  ws.MODULE_SCOPE = pbnj.core; // default scope
  pbnj.core["@@NAME@@"] = _.symbol('pbnj.core');
  pbnj.core["@@MACROS@@"] = {};

  var globalEnv = pbnj.core['@@SCOPE@@'] = pbnj.env().setSource('src/pbnj/wonderscript.js');
  globalEnv.define('*module-name*', _.symbol('pbnj.core'));
  importModule(pbnj.core);
  ws.globalEnv = globalEnv;
  
  globalEnv.define('*source*', null);
  globalEnv.define('identical?', function(a, b) { return a === b; });
  globalEnv.define('equiv?', function(a, b) { return a == b; });
  globalEnv.define('not', function(x) { return !x; });
  globalEnv.define('=', _.equals);
  globalEnv.define('>', function(a, b) { return a > b; });
  globalEnv.define('<', function(a, b) { return a < b; });
  globalEnv.define('<=', function(a, b) { return a <= b; });
  globalEnv.define('>=', function(a, b) { return a >= b; });
  globalEnv.define('mod', function(a, b) { return a % b; });
  globalEnv.define('array', function() { return Array.prototype.slice.call(arguments); });
  globalEnv.define('object', function() { return {} });
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
    else if (_.isString(exp)) {
      return _.str('"', exp, '"');
    }
    else if (isQuoted(exp)) {
      return _.str("'", ws.inspect(_.second(exp)));
    }
    else if (_.isArray(exp)) {
      var buffer = [];
      for (var i = 0; i < exp.length; i++) {
        buffer.push(ws.inspect(exp[i]));
      }
      return _.str("(array ", buffer.join(' '), ")");
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
    else if (_.isKeyword(exp)) {
      return evalKeyword(exp, env);
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
    else if (isCond(exp)) {
      return evalCond(exp, env);
    }
    else if (isLambda(exp)) {
      return evalLambda(exp, env);
    }
    else if (isBlock(exp)) {
      return evalBlock(exp, env);
    }
    else if (isTryBlock(exp)) {
      return evalTryBlock(exp, env);
    }
    else if (isLoop(exp)) {
      return evalLoop(exp, env);
    }
    else if (isRecursionPoint(exp)) {
      return evalRecursionPoint(exp, env);
    }
    else if (isAssignment(exp)) {
      return evalAssignment(exp, env);
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
    else if (isPropertyAssignment(exp)) {
      return evalPropertyAssignment(exp, env);
    }
    else if (isMethodApplication(exp)) {
      return evalMethodApplication(exp, env);
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
    else if (isModuleSet(exp)) {
      return evalModuleSet(exp, env);
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
    var env = (env || globalEnv).extend().setSource(stream.source()).setLocation(stream.line(), stream.column());
    try {
      var value = null;
      while (!stream.eof()) {
        var exp = stream.peek();
        value = ws.eval(exp, env);
        if (_.isFunction(value) || isBlock(exp) || isTryBlock(exp)) {
          env.setLocation(stream.line(), stream.column());
        }
        else if (value && value["@@SCOPE@@"]) {
          env = value["@@SCOPE@@"].setLocation(stream.line(), stream.column());
        }
        else {
          env.setLocation(stream.line(), stream.column());
        }
        stream.next();
      }
      return value;
    }
    catch(e) {
      console.log(e);
      console.log(env);
      console.log(fmtStacktrace(env.stacktrace()));
      throw e; //new Error(wsError(e, env.stacktrace()));
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

  ws.evalJS = function(exp) {
    //console.log(str);
    var data = pbnj.reader.readJS(exp);
    return ws.eval(data);
  };
  globalEnv.define('eval-js', ws.evalJS);

  ws.compileString = function(str, input) {
    var stream = pbnj.reader.readString(str, input);
    return ws.compileStream(stream);
  };
  globalEnv.define('compile-string', ws.compileString);

  ws.readFile = function(file, env) {
    var env = env || globalEnv;
    scope = env.extend().setIdent('read-file');
    scope.define('*file*', file);
    var path = file.split('/');
    scope.define('*dir*', path.slice(0, path.length - 1).join('/'));
    var stream = pbnj.reader.readFile(file);
    return ws.readStream(stream, scope);
  };
  globalEnv.define('read-file', ws.readFile);

  ws.compileFile = function(file) {
    var stream = pbnj.reader.readFile(file);
    return ws.compileStream(stream);
  };
  globalEnv.define('compile-file', ws.compileFile);

  globalEnv.define('*environment*', _.keyword('development'));
  // TODO: detect browser?
  globalEnv.define('*platform*', typeof exports !== 'undefined' ? 'nodejs' : 'browser');

  if (typeof exports !== 'undefined') {
    module.exports = ws;
    var mod = defineModule(_.symbol('js.node'));
    [
      'Buffer',
      '__dirname',
      '__filename',
      'clearImmediate',
      'clearInterval',
      'clearTimeout',
      'console',
      'exports',
      'global',
      'module',
      'process',
      'require',
      'setImmediate',
      'setInterval',
      'setTimeout'
    ].forEach(function(name) {
      defineVariable(globalEnv, _.symbol('js.node', name), eval(name));
    });
  }
  ws.readFile("src/pbnj/core.ws");

} // namespace pbnj.wonderscript
