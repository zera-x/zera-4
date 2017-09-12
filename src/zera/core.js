// jshint esversion: 5
// jshint eqnull: true
// jshint evil: true
// jshint shadow: true
// jshint boss: true
var zera = zera || {};
zera.core = {};
(function() {
  'use strict';
  var ws = zera.core;

  var isNode    = false;
  var isBrowser = false;
  var isJSA     = false;
  var isNashorn = false;

  var _, ROOT_OBJECT, mori;
  if (typeof exports !== 'undefined') {
    isNode = true;
    mori = require('mori');
    _    = require('./util.js');
    zera.util = _;
    zera.reader = require('./reader.js');
    ROOT_OBJECT = global;
  }
  else if (typeof java !== 'undefined') {
    isNashorn = true;
  }
  else if (typeof ObjC !== 'undefined') {
    isJSA = true;
  }
  else if (typeof document !== 'undefined') {
    isBrowser = true;
    mori = window.mori;
    ROOT_OBJECT = window;
    _ = ROOT_OBJECT.zera.util;
  }
  else {
    console.warn('Unkown JavaScript environment');
  }

  ROOT_OBJECT.zera = ROOT_OBJECT.zera || {};
  ROOT_OBJECT.zera.core = ws;
  ROOT_OBJECT.zera.util = _;
  ROOT_OBJECT.zera.reader = zera.reader;

  ws.STREAM_META = zera.reader.STREAM_META.swap(_.assoc, _.keyword('source'), "src/zera/core.ts");
  ws.inspect = _.inspect;

  // Exceptions
  //
  function UndefinedVariableException(ident) {
      this.name = "UndefinedVariableException";
      this.message = ["Undefined variable: '", ident, "'"].join('');
      //this.stack = (new Error()).stack;
  }
  UndefinedVariableException.prototype = Object.create(Error.prototype);
  UndefinedVariableException.prototype.constructor = UndefinedVariableException;
  ws.UndefinedVariableException = UndefinedVariableException;

  // types
  //

  ws.Protocol = _.makeType(
    _.symbol('zera.core', 'Protocol'),
    _.hashMap(),
    function(methods, protocols) {
      this.methods   = methods;
      this.protocols = protocols;
    }
  );

  ws.Type = _.makeType(
    _.symbol('zera.core', 'Type'),
    _.hashMap(),
    function(attrs, methods, protocols) {
      this.attrs     = attrs;
      this.methods   = methods;
      this.protocols = protocols;
    }
  );

  ws.Function = _.makeType(
    _.symbol('zera.core', 'Function'),
    _.hashMap(_.keyword('types'), _.set([ws.Protocol])),
    function() {}
  );

  ws.Block = _.makeType(
    _.symbol('zera.core', 'Block'),
    _.hashMap(_.keyword('types'), _.set([ws.Protocol])),
    function() {}
  );

  ws.Object = _.makeType(
    _.symbol('zera.core', 'Object'),
    _.hashMap(_.keyword('types'), _.set([ws.Protocol])),
    function() {}
  );

  ws.Ref = _.makeType(
    _.symbol('zera.core', 'Ref'),
    _.hashMap(_.keyword('types'), _.set([ws.Protocol, ws.Object])),
    function() {}
  );

  ws.env = function(parent) {
    return new Env(parent);
  };

  var ENV_ID = 0;

  /**
   * @constructor
   * @final
   */
  var Env = _.makeType(
    _.symbol('zera.core', 'Env'),
    _.hashMap(_.keyword('type'), _.set([ws.Object])),
    function Env(parent, meta) {
      this.meta  = meta || _.hashMap(); 
      this.vars = Object.create(parent ? parent.vars : null);
      this.parent = parent;
      this.define('*scope*', this);
      this.id = ENV_ID++;
    }
  );

  Env.prototype.getID = function() {
    return this.id;
  };

  Env.prototype.getMeta = function() {
    return this.meta;
  };

  Env.prototype.withMeta = function(meta) {
    this.meta = meta;
    return this;
  };

  Env.prototype.varyMeta = function(f, args) {
    var args_ = args || _.list();
    return this.withMeta(ws.apply(f, _.cons(this.meta, args_)));
  };

  Env.prototype.extend = function(meta) {
    return new Env(this, meta);
  };

  Env.prototype.lookup = function(name) {
    var scope = this;
    while (scope) {
      if (Object.prototype.hasOwnProperty.call(scope.vars, name)) {
        return scope;
      }
      scope = scope.parent;
    }
    return null;
  };

  var FETCHED = {};
  Env.prototype.fetchObject = function(name) {
    if (FETCHED[name]) return FETCHED[name];
    else {
      var env = this.lookup(name);
      if (env) {
        var obj = env.getObject(name);
        FETCHED[name] = obj;
        return obj;
      }
      else {
        return null;
      }
    }
  };

  Env.prototype.getObject = function(name) {
    if (name in this.vars) {
      return this.vars[name];
    }
    throw new Error(_.str("Undefined variable: '", name, "' in Env: ", this.id));
  };

  Env.prototype.getScopeObjects = function() {
    var buffer = [];
    var scope = this;
    var props, prop, i;
    while (scope) {
      props = Object.getOwnPropertyNames(scope.vars);
      for (i = 0; i < props.length; i++) {
        prop = props[i];
        buffer.push(_.vector(_.symbol(prop), scope.vars[prop]));
      }
      scope = scope.parent;
    }
    return mori.into(_.hashMap(), buffer);
  };

  Env.prototype.getObjects = function() {
    var buffer = [];
    var props, prop, i;
    props = Object.getOwnPropertyNames(this.vars);
    for (i = 0; i < props.length; i++) {
      prop = props[i];
      buffer.push(_.vector(_.symbol(prop), this.vars[prop]));
    }
    return mori.into(_.hashMap(), buffer);
  };

  Env.prototype.toString = function() {
    return _.str("#<Env id: ", this.id, ", ident: ", this.ident, ", vars: ",
      _.reduce(function(s, x) { return _.str(s, ', ', x); }, _.sort(_.map(_.first, this.getObjects()))), ">");
  };

  Env.prototype.get = function(name) {
    var obj = this.getObject(name);
    return obj.get();
  };

  Env.prototype.set = function(name, value) {
    var scope = this.lookup(name);
    // cannot define globals from a nested environment
    if (!scope && this.parent) throw new Error(_.str("Undefined variable: '", name, "'"));
    (scope || this).vars[name].set(value);
    return value;
  };

  var typeTag = function(value) {
    if (_.isFunction(value)) {
      if (value.$lang$ws$tag) {
        return value.$lang$ws$tag;
      }
      return _.keyword('js', 'Function');
    }
    else if (_.isKeyword(value)) {
      return _.keyword('keyword');
    }
    else if (_.isSymbol(value)) {
      return _.keyword('symbol');
    }
    else if (_.isCollection(value)) {
      if (_.isList(value)) {
        return _.keyword('list');
      }
      else if (_.isVector(value)) {
        return _.keyword('vector');
      }
      else if (_.isMap(value)) {
        return _.keyword('map');
      }
      else if (_.isSet(value)) {
        return _.keyword('set');
      }
      else {
        return _.keyword('collection');
      }
    }
    else if (_.isArray(value)) {
      return _.keyword('js', 'Array');
    }
    else if (_.isObject(value)) {
      var nm = value.constructor && value.constructor.name;
      if (nm === 'Object' || nm === '' || nm == null) {
        return _.keyword('js', 'Object');
      }
      else {
        return _.keyword('js', nm);
      }
    }
    else {
      return typeof value;
    }
  };

  Env.prototype.importVars = function(env) {
    var vars = env.vars;
    var scope;
    for (var name in vars) {
      if (Object.prototype.hasOwnProperty.call(vars, name)) {
        if ((scope = this.lookup(name)) && scope.parent) {
          console.warn(['WARNING: ', name, ' exists in ', scope.getIdent(), ' it will be shadowed in this module'].join(''));
        }
        this.vars[name] = vars[name];
      }
    }
  };

  Env.prototype.define = function(name, value, meta) {
    var sname = _.str(name);
    var meta  = _.merge(_.hashMap(_.keyword('name'), _.symbol(sname), _.keyword('tag'), typeTag(value)), meta || _.hashMap());
    var m     = _.hashMap();

    if (_.isFunction(value)) {
      if (value.arglists) {
        m = _.assoc(m, _.keyword('arglists'), value.arglists());
      }
      var hash;
      if (hash = _.hasKey(meta, _.keyword('memoize'))) {
        var enclosed = value;
        var hasher = _.isFunction(hash) ? hash : null;
        value = function(key) {
          var ret;
          if (ret = _.get(value.memo, key)) {
            return ret;
          }
          else {
            var address = hasher ? hasher.apply(this, arguments) : key;
            ret = enclosed.apply(this, arguments);
            value.memo = _.assoc(value.memo, address, ret);
            return ret;
          }
        };
        value.memo = _.hashMap();
      }
    }

    this.vars[sname] = new _.Var(value, _.merge(m, meta));
    return value;
  };

  ws['var'] = function(name, env) {
    var env = env || globalEnv;
    return ws.lookupVar(name, env);
  };

  ws.isEnv = function(val) {
    return val instanceof Env;
  };

  ws.Env = Env;

  // Exceptions
  ws.ArgumentException = function() {
    this.message = _.str();
  };

  //
  // Evaluator
  //

  var makeTagPredicate = _.makeTagPredicate;

  var isSelfEvaluating = ws.isSelfEvaluating = function(exp) {
    return _.isNull(exp) ||
           _.isUndefined(exp) ||
           _.isBoolean(exp) ||
           _.isString(exp) ||
           _.isNumber(exp) ||
           _.isDate(exp) ||
           _.isRegExp(exp);
  };

  var evalSelfEvaluating = _.identity;

  var evalKeyword = function(exp, env) {
    var sexp = '' + exp;
    if (sexp.startsWith('::')) {
      var modname = ws.NS_SCOPE.getName();
      return _.keyword('' + modname, sexp.replace(/^::/, ''));
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
                function(xs) { return mori.vector(ws.eval(mori.nth(xs, 0), env), ws.eval(mori.nth(xs, 1), env)); },
                exp));
    }
    else if (_.isVector(exp)) {
      return mori.into(mori.vector(),
              mori.map(
                function(x) { return ws.eval(x, env); },
                exp));
    }
    else if (_.isSet(exp)) {
      return mori.into(mori.set(),
              mori.map(
                function(x) { return ws.eval(x, env); },
                exp));
    }
    else {
      throw new Error(['invalid form: ', exp].join(''));
    }
  };

  var isQuoted = ws.isQuoted = makeTagPredicate(_.symbol('quote'));

  var evalQuote = _.second;

  var isVariable = ws.isVariable = _.isSymbol;

  ws.lookupVar = function(ident, env) {
    if (!_.isSymbol(ident)) throw new Error(['ident should be a symbol, got: ', _.inspect(ident)].join(''));
    var scope, sym, ns, nm;
    if (isFullyQualified(ident)) {
      var nsvar = ws.lookupVar(_.symbol(_.namespace(ident)), env);
      if (nsvar.isNamespace() || nsvar.isAlias()) {
        var obj = nsvar.get().getVar(_.name(ident));
        return obj;
      }
      else {
        throw new Error(_.str('invalid namespace: ', _.namespace(ident)));
      }
    }
    else {
      if (scope = env.lookup(ident)) {
        sym = ident;
      }
      else {
        scope = ws.NS_SCOPE.lookup(ident);
        sym = ident;
        if (scope === null) {
          scope = ws.DEFAULT_NS.lookup(sym);
        }
      }
    }

    if (scope === null) throw new zera.core.UndefinedVariableException(ident);
    else {
      return scope.getObject(sym);
    }
  };

  var evalVariable = function(exp, env) {
    var v = ws.lookupVar(exp, env);
    if (v.isMacro()) throw new Error(["Can't take macro as a value: (var '", exp, ")"].join(''));
    return v.get();
  };

  var isDefinition = ws.isDefinition = makeTagPredicate(_.symbol('def'));

  var defineVariable = function(env, ident, value, meta) {
    var meta = meta || _.hashMap();
    var isPrivate = _.get(meta, _.keyword('private'), false);
    if (!isPrivate) {
      // define value in module scope
      var qualified = isFullyQualified(ident) ? ident : qualifySymbol(ident);
      var ns        = ws.Namespace.get(_.symbol(_.namespace(qualified)));
      meta = _.assoc(meta, _.keyword('namespace'), ns);
      ns.define(_.symbol(_.name(qualified)), value, meta);
    }
    env.define(ident, value, meta);
    return env;
  };
  ws.defineVariable = defineVariable;

  var qualifySymbol = function(sym) {
    var ns   = _.namespace(sym);
    var name = _.name(sym);
    if (ns) {
      return sym;
    }
    else {
      return _.symbol(ws.NS_SCOPE.getName(), name);
    }
  };
  ws.qualifySymbol = ws['qualify-symbol'] = qualifySymbol;

  var isFullyQualified = function(sym) {
    return _.namespace(sym);
  };
  ws['fully-qualified?'] = isFullyQualified;

  var evalDefinition = function(exp, env) {
    var rest  = _.rest(exp);
    var first = _.first(rest);
    var meta, ident, preVal;
    if (_.isSymbol(first)) {
      meta   = _.hashMap();
      ident  = first;
      preVal = _.second(rest);
    }
    else if (_.isMap(first)) {
      meta   = ws.eval(first, env);
      ident  = _.first(_.rest(rest));
      preVal = _.second(_.rest(rest));
    }
    else if (_.isKeyword(first)) {
      meta   = _.hashMap(first, true);
      ident  = _.first(_.rest(rest));
      preVal = _.second(_.rest(rest));
    }
    else {
      throw new Error(['define: first element should be a symbol, keyword, or map from: ', ws.inspect(exp)].join(''));
    }

    var name = _.name(ident);
    var ns   = _.namespace(ident);
    
    env.define(name); // so definition can refer to itself

    var value = preVal == null ? null : ws.eval(preVal, env);
    defineVariable(env, ident, value, meta);

    if (_.isFunction(value)) {
      value.$lang$ws$ident = ident;
    }
    return value;
  };

  var isProtocol = ws.isProtocol = makeTagPredicate(_.symbol('defprotocol'));

  // (distance [self other] ...)
  // TODO: change type/protocol polymorphisim to:
  //   (deftype Test [a b]
  //      (test [this] "Test")
  //      (test [this x] (str "Test" x)))
  var evalMethod = function(exp, env) {
    var lambda, name, expr, meth;
    name = _.first(exp);
    expr = _.cons(_.symbol('fn'), _.rest(exp));
    //ws.pprint(expr);
    lambda = evalLambda(expr, env);
    meth = function() {
      return lambda.apply(null, [].concat([this], Array.prototype.slice.call(arguments)));
    };
    var prop;
    for (prop in lambda) {
      if (lambda.hasOwnProperty(prop)) {
        meth[prop] = lambda[prop];
      }
    }
    meth.toString = function() {
      return ws.inspect(exp);
    };
    meth.methodName = function() {
      return name;
    };
    return [name, meth];
  };

  // (defprotocol IPoint
  //  (distance [self] ...))
  // (deftype Point [x y] IPoint)
  var evalProtocol = function(exp, env) {
    var rest, name, specs, mixin, methods, types, mixins, doc, meta;
    rest = _.rest(exp);
    name = _.first(rest);

    if (!_.isSymbol(name)) throw new Error('name should be a symbol');
    
    if (_.isString(doc = _.second(rest)) && _.isMap(meta = _.first(_.rest(_.rest(rest))))) {
      specs = _.intoArray(_.rest(_.rest(_.rest(rest))));
    }
    else if (_.isString(doc)) {
      meta = null;
      specs = _.intoArray(_.rest(_.rest(rest)));
    }
    else if (_.isMap(meta)) {
      doc = null;
      specs = _.intoArray(_.rest(_.rest(rest)));
    }
    else {
      doc = meta = null;
      specs = _.intoArray(_.rest(rest));
    }

    methods = [];
    types   = _.set();
    mixins  = [];

    if (specs.length !== 0) {
      for (i = 0; i < specs.length; i++) {
        if (_.isSymbol(specs[i])) {
          mixin = ws.eval(specs[i], env);
          types = _.conj(types, mixin);
          mixins.push(mixin);
        }
        else if (_.isList(specs[i])) {
          methods.push(evalMethod(specs[i], env));
        }
        else {
          console.log(specs[i]);
          throw new Error('spec should be either a protocol name or a method definition');
        }
      }
    }

    mixin = function(obj) {
      if (arguments.length !== 1) {
        throw new Error(['1 arguments expected, got: ', arguments.length].join(''));
      }
      if (mixins.length !== 0) {
        for (var i = 0; i < mixins.length; i++) {
          mixins[i].call(null, obj);
        }
      }
      if (methods.length !== 0) {
        for (var i = 0; i < methods.length; i++) {
          obj[methods[i][0]] = methods[i][1];
        }
      }
      return obj;
    };

    mixin.types = function() {
      return types;
    };

    mixin.toString = function() {
      return ['#<Protocol: ', name.toString(), '>'].join('');
    };

    mixin.instanceMethods = function() {
      return _.map(function(x) { return x[1]; }, methods);
    };

    var meta_ = _.hashMap(_.keyword('protocol'), true);
    if (meta) meta_ = _.merge(meta_, meta);
    if (doc) meta_ = _.assoc(meta_, _.keyword('doc'), doc);

    mixin.getMeta = function() {
      return meta_;
    };

    defineVariable(env, name, mixin, meta_);

    return mixin;
  };

  var isType = ws.isType = makeTagPredicate(_.symbol('deftype'));

  // (define-type Point [x y])
  var evalType = function(exp, env) {
    var rest, name, fields, specs, argc, names, ctr, mixin, method, i, doc, meta;
    rest = _.rest(exp);
    name = _.first(rest);
    if (!_.isSymbol(name)) throw new Error('name should be a symbol');

    if (_.isVector(fields = _.second(rest))) {
      specs = _.intoArray(_.rest(_.rest(rest)));
    }
    else if (_.isString(doc = fields) && _.isMap(meta = _.first(_.rest(_.rest(rest))))) {
      fields = _.first(_.rest(_.rest(_.rest(rest))));
      specs = _.intoArray(_.rest(_.rest(_.rest(_.rest(rest)))));
    }
    else if (_.isString(doc = fields)) {
      meta = null;
      fields = _.first(_.rest(_.rest(rest)));
      specs = _.intoArray(_.rest(_.rest(_.rest(rest))));
    }
    else if (_.isMap(meta = fields)) {
      doc = null;
      fields = _.first(_.rest(_.rest(rest)));
      specs = _.intoArray(_.rest(_.rest(_.rest(rest))));
    }

    if (!_.isVector(fields)) throw new Error('field list should be a vector of symbols');

    argc  = _.count(fields);
    names = _.intoArray(fields);
    ctr = function() {
      if (arguments.length !== argc) {
        throw new Error(['expected ', argc, ' arguments, got: ', arguments.length].join(''));
      }
      for (i = 0; i < names.length; i++) {
        this[names[i]] = arguments[i];
      }
    };

    var types = _.set([ctr]);
    var methods = [];

    if (specs.length !== 0) {
      ctr.prototype = {};
      for (i = 0; i < specs.length; i++) {
        if (_.isSymbol(specs[i])) {
          mixin = ws.eval(specs[i], env);
          types = _.union(_.conj(types, mixin), mixin.types());
          // TODO: perform checks based on meta data
          mixin.call(null, ctr.prototype);
        }
        else if (_.isList(specs[i])) {
          method = evalMethod(specs[i], env);
          methods.push(method);
          ctr.prototype[method[0]] = method[1];
        }
        else {
          console.log(specs[i]);
          throw new Error('spec should be either a protocol name or a method definition');
        }
      }
    }

    ctr.prototype.toString = function() {
      var that = this;
      var vars = _.reduce(function(s, x) { return [s, ', ', x].join(''); },
                    _.map(function(x) { return [x, ': ', ws.inspect(that[x])].join(''); }, names));
      if (names.length === 0) {
        return ["#<", name, ">"].join('');
      }
      return ["#<", name, " ",  vars, ">"].join('');
    };

    ctr.prototype.types = function() {
      return types;
    };

    ctr.prototype['class'] = function() {
      return ctr;
    };

    ctr.prototype.isa = function(klass) {
      return _.has(types, klass);
    };

    var meta_ = _.hashMap(_.keyword('type'), true, _.keyword('arglists'), _.list(fields));
    if (meta) meta_ = _.merge(meta_, meta);
    if (doc) meta_ = _.assoc(meta_, _.keyword('doc'), doc);

    ctr.getMeta = function() {
      return meta_;
    };

    ctr.toString = function() {
      return name.toString();
    };

    ctr.instanceMethods = function() {
      return _.map(function(x) { return x[1]; }, methods);
    };

    defineVariable(env, name, ctr, meta_);

    return name;
  };

  var arity = function(fn) {
    return fn.$lang$ws$arity || fn.length;
  };

  var funcIdent = function(func) {
    return func.$lang$ws$ident || func.name || 'anonymous';
  };

  var isInvocable = function(val) {
    return _.isFunction(val) || _.isAssociative(val) || _.isKeyword(val) || _.isSet(val) || (isQuoted(val) && _.isSymbol(evalQuoted(val)));
  };

  ws.apply = function(func, args) {
    if (arguments.length !== 1 && arguments.length !== 2) {
      throw new Error(['wrong number of arguments expected: 1 or 2 arguments, got: ', arguments.length].join(''));
    }

    if (!isInvocable(func)) {
      throw new Error(["'", func, "' is not invocable"].join(''));
    }

    return func.apply(null, args ? _.intoArray(args) : []);
  };

  var isLambda = ws.isLambda = makeTagPredicate(_.symbol('fn'));

  /*
  var validateVariables = function(body, scope) {
    var exp_, exp, list = body;
    while(exp = _.first(list)) {
      var sexp = '' + exp;
      if (_.isSymbol(exp) && !(_.has(ws.TAGGED_SPECIAL_FORMS, exp) || sexp.endsWith('.') || sexp.startsWith('.') || sexp.startsWith('.-'))) {
        var val = evalVariable(exp, scope);
        scope.define(exp, val, _.hashMap(_.keyword('dynamic'), true));
      }
      else if (_.isList(exp)) {
        exp_ = macroexpand(exp);
        if (isDefinition(exp_)) {
          //console.log('def');
          scope.define(_.second(_.rest(exp_)), null, _.hashMap(_.keyword('dynamic'), true));
        }
        else if (isBlock(exp_)) {
          //console.log('block');
          validateVariables(_.rest(exp_), scope);
        }
        else if (isLoop(exp_)) {
          //console.log('loop');
          var names = _.map(_.first, _.partition(2, _.second(exp_)));
          _.each(names, function(name) {
            scope.extend().define(name, null, _.hashMap(_.keyword('dynamic'), true));
          });
        }
        else if (isLambda(exp_)) {
          var names = _.second(exp_);
          if (_.isList(names)) {
            names = _.mapcat(_.first, _.rest(exp_));
          }
          _.each(names, function(name) {
            scope.extend().define(name, null, _.hashMap(_.keyword('dynamic'), true));
          });
        }
        else if (isPropertyAccessor(exp_)) {
          // skip
        }
        else if (isPropertyAssignment(exp_)) {
          // skip
        }
        else {
          //validateVariables(exp_, scope);
        }
      }
      else if (_.isVector(exp) || _.isSet(exp)) {
        validateVariables(exp, scope);
      }
      else if (_.isMap(exp)) {
        validateVariables(_.keys(exp), scope);
        validateVariables(_.vals(exp), scope);
      }
      list = _.rest(list);
    }
    return body;
  };*/

  var lambdaID = 0;
  var Lambda = _.makeType('Lambda', _.hashMap(), function Lambda(exp, env, callstack) {
    var id, rest, names, argCount, bodies, i, fn, exprs, args, body, sum, keys, arities;

    if (arguments.length < 2) {
      throw new Error(['at least 2 arguments expected got: ', arguments.length].join(''));
    }

    id = ['lambda-', lambdaID++].join('');

    this.callstack = callstack || ws.stack();
    this.callstack.swap(_.conj, trace(_.symbol(id)));

    rest = _.rest(exp);
    names = _.first(rest);
    argCount = 0;
    bodies = {};

    this.scope = env.extend();

    if (_.isVector(names)) {
      argCount = _.count(names);
      for (var i = 0; i < argCount; i++) {
        var name  = _.nth(names, i);
        var sname = '' + name;
        if (sname[0] === '&') {
          argCount = (argCount - 1) * -1;
          //this.scope.define(_.symbol(sname.slice(1)));
        }
        else {
          //this.scope.define(name);
        }
      }
      bodies[argCount] = {arity: argCount, args: names, code: _.rest(rest)};
      var body = bodies[argCount].code;
    }
    else if (_.isList(names)) {
      exprs = rest;
      while (fn = _.first(exprs)) {
        args = _.intoArray(_.first(fn));
        body = _.rest(fn);
        sum = 0;
        for (i = 0; i < args.length; i++) {
          var name = args[i];
          var sname = '' + name;
          if (sname[0] === '&') {
            sum = (sum + 1) * -1;
            //this.scope.define(_.symbol(sname.slice(1)));
          }
          else {
            sum++;
            //this.scope.define(name);
          }
        }
        bodies[sum] = {arity: sum, args: args, code: body};
        exprs = _.rest(exprs);
      }
      keys = _.keys(bodies);
      arities = [];
      for (i = 0; i < keys.length; i++) {
        arities.push(Math.abs(bodies[keys[i]].arity));
      }
      argCount = _.sort(arities)[0] * -1;
    }
    else {
      throw new Error('the second element of a lambda expression should be an argument vector or the beginning of a list of lambda bodies');
    }
    this.bodies = function() {
      return bodies;
    };
    this.arglists = function() {
      return _.map(function(body) { return _.isArray(body.args) ? _.vector.apply(null, body.args) : body.args; }, _.values(bodies));
    };
    this.id = function() {
      return id;
    };
    this.arity = function() {
      return argCount;
    };
    this.length = argCount;
    this.toString = function() {
      return ws.inspect(exp);
    };
  });

  Lambda.prototype.bind = function(values) {
    var bodies = this.bodies();
    var scope = this.scope.extend();

    var args = values;
    var argCount = this.arity();
    if (argCount <= 0 && args.length < Math.abs(argCount)) {
      throw new Error(['"', this, '" wrong number of arguments expected at least: ', Math.abs(argCount), ' got: ', args.length].join(''));
    }
    else if (argCount > 0 && argCount !== args.length) {
      throw new Error(['"', this, '" wrong number of arguments expected: ', argCount, ' got: ', args.length].join(''));
    }
    var argc = args.length;
    var body = bodies[argc];
    if (body == null) {
      for (var i = (argc * -1); i <= 0; i++) {
        body = bodies[i];
        if (body != null) break;
      }
      if (body == null) throw new Error(["wrong number of arguments for: ", this, ' got: ', args.length].join(''));
    }
    var i = 0;
    var bodyArgs = body.args;
    var name;
    while (name = _.first(bodyArgs)) {
      var sname = '' + name;
      if (sname[0] === '&') {
        var list = _.list.apply(null, [].slice.call(args, i));
        scope.define(sname.slice(1), list);
      }
      else {
        scope.define(sname, i < args.length ? args[i] : false);
      }
      i++;
      bodyArgs = _.rest(bodyArgs);
    }
    this.scope = scope;
    this.body = body;
    return this;
  };

  Lambda.prototype.exec = function() {
    var body = this.body;
    if (!body) throw new Error('the lambda must be bound to arguments first');
    var exprs = body.code, exp, ret, index = 0;
    while (_.count(exprs) !== 0) {
      exp = _.first(exprs);
      try {
        ret = ws.eval(exp, this.scope);
        index++; // TODO: add static analysis to index just what's needed
      }
      catch (e) {
        if (e instanceof RecursionPoint) {
          return this.bind(e.args).exec();
        }
        else {
          throw e;
        }
      }
      exprs = _.rest(exprs);
    }
    return ret;
  };

  Lambda.prototype.apply = function(obj, args) {
    return this.bind(args).exec();
  };

  Lambda.prototype.call = function() {
    return this.bind(_.toArray(arguments).slice(1)).exec();
  };

  Lambda.prototype.toFunction = function() {
    var that = this;
    var fn = function() {
      return that.bind(arguments).exec();
    };
    var prop;
    for (prop in this) {
      if (this.hasOwnProperty(prop)) {
        if (prop === 'length') continue;
        fn[prop] = this[prop];
      }
    }
    return fn;
  };

  ws['fn?'] = _.isFunction;

  var evalLambda = function(exp, env, callstack) {
    return new Lambda(exp, env, callstack).toFunction();
  };

  var isLoop = ws.isLoop = makeTagPredicate(_.symbol('loop'));
  var isRecursionPoint = ws.isRecursionPoint = makeTagPredicate(_.symbol('recur'));

  var Loop = _.makeType('Loop', _.hashMap(), function Loop(bindings, body, env) {
    if (arguments.length !== 3) {
      throw new Error(['3 arguments expected got: ', arguments.length].join(''));
    }
    this.bindings = function() {
      return bindings;
    };
    this.body = function() {
      return body;
    };
    this.scope = env.extend();
    var names = [];
    for (var i = 0; i < _.count(bindings); i += 2) {
      var name = _.nth(bindings, i);
      names.push(name);
      if (env) {
        this.scope.define(name);
        this.scope.define(name, ws.eval(_.nth(bindings, i + 1), this.scope));
      }
    }
    this.names = function() {
      return names;
    };
  });

  Loop.prototype.bind = function(values) {
    var names = this.names();
    var scope = this.scope.extend();
    if (names.length !== values.length) {
      throw new Error('values should match bindings arity');
    }
    for (var i = 0; i < names.length; i++) {
      scope.define(names[i]);
      scope.define(names[i], values[i]);
    }
    this.scope = scope;
    return this;
  };

  Loop.prototype.exec = function(env) {
    var exp, ret, exprs = this.body();
    while (_.count(exprs) !== 0) {
      try {
        exp = _.first(exprs);
        ret = ws.eval(exp, env ? env : this.scope);
        exprs = _.rest(exprs);
      }
      catch (e) {
        if (e instanceof RecursionPoint) {
          this.bind(e.args).exec();
        }
        else {
          throw e;
        }
      }
    }
    return ret;
  };

  var evalLoop = function(exp, env) {
    var bindings = _.second(exp);
    var body = _.rest(_.rest(exp));
    if (!_.isVector(bindings)) throw new Error('bindings should be a vector');
    return new Loop(bindings, body, env).exec();
  };

  function RecursionPoint(args) {
    this.args = args;
  }

  // TODO: add tail position check
  var evalRecursionPoint = function(exp, env) {
    var args = _.intoArray(mori.map(function(x) { return ws.eval(x, env); }, _.rest(exp)));
    throw new RecursionPoint(args);
  };

  var isApplication = ws.isApplication = function(exp) {
    return _.isList(exp);
  };

  var trace = function(ident) {
    var meta = ws.STREAM_META.deref();
    return _.vector(
      _.get(meta, _.keyword('expression')),
      _.get(meta, _.keyword('source')),
      _.get(meta, _.keyword('line')),
      _.get(meta, _.keyword('column'))
    );
  };

  var evalApplication = ws.evalApplication = function(exp, env, callstack) {
    var func, args, rawargs, buffer, op, op_, sop, newExp, obj, meth, ctr, i;

    args    = [];
    rawargs = _.intoArray(_.rest(exp));
    for (i = 0; i < rawargs.length; i++) {
      args.push(ws.eval(rawargs[i], env));
    }

    // Primitive operators are inlined for performance
    op = _.first(exp);

    var callstack = callstack || ws.stack();
    callstack.swap(_.conj, trace(op));

    if (_.equals(op, _.symbol('+'))) {
      if (args.length === 0) return 0;
      return eval(['(', args.join(')+('), ')'].join(''));
    }
    else if (_.equals(op, _.symbol('-'))) {
      if (args.length === 0) return 0;
      else if (args.length === 1) return eval(['-', args[0]].join(''));
      return eval(['(', args.join(')-('), ')'].join(''));
    }
    else if (_.equals(op, _.symbol('*'))) {
      if (args.length === 0) return 1;
      return eval(['(', args.join(')*('), ')'].join(''));
    }
    else if (_.equals(op, _.symbol('/'))) {
      if (args.length === 0) return 1;
      return eval(['(', args.join(')/('), ')'].join(''));
    }
    else if (_.equals(op, _.symbol('str'))) {
      if (args.length === 0) return '';
      else if (args.length === 1) return '' + args[0];
      return args.join('');
    }
    else if (_.equals(op, _.symbol('add1'))) {
      if (args.length !== 1) {
        throw new Error(['expected 1 argument, got: ', args.length].join(''));
      }
      else {
        return eval(['1+', args[0]].join(''));
      }
    }
    else if (_.equals(op, _.symbol('sub1'))) {
      if (args.length !== 1) {
        throw new Error(['expected 1 argument, got: ', args.length].join(''));
      }
      else {
        return eval([args[0], '-1'].join(''));
      }
    }
    else if (_.equals(op, _.symbol('not'))) {
      if (args.length !== 1) {
        throw new Error(['expected 1 argument, got: ', _.coargs.length].join(''));
      }
      else {
        if (args[0] == null || args[0] === false) {
          return true;
        }
        else {
          return false;
        }
      }
    }
    else if (_.equals(op, _.symbol('mod'))) {
      return eval(['(', args.join(')&('), ')'].join(''));
    }
    else if (_.equals(op, _.symbol('bit-or'))) {
      return eval(['(', args.join(')|('), ')'].join(''));
    }
    else if (_.equals(op, _.symbol('bit-and'))) {
      return eval(['(', args.join(')&('), ')'].join(''));
    }
    else if (_.equals(op, _.symbol('bit-xor'))) {
      return eval(['(', args.join(')^('), ')'].join(''));
    }
    else if (_.equals(op, _.symbol('bit-not'))) {
      if (args.length !== 1) {
        throw new Error(['expected 1 argument, got: ', args.length].join(''));
      }
      else {
        return eval(['~', args.length].join(''));
      }
    }
    else if (_.equals(op, _.symbol('bit-shift-left'))) {
      if (args.length !== 2) {
        throw new Error(['expected 2 argument, got: ', args.length].join(''));
      }
      else {
        return eval([args[0], '<<', args[1]].join(''));
      }
    }
    else if (_.equals(op, _.symbol('bit-shift-right'))) {
      if (args.length !== 2) {
        throw new Error(['expected 2 argument, got: ', args.length].join(''));
      }
      else {
        return eval([args[0], '>>', args[1]].join(''));
      }
    }
    else if ((sop = op.toString()).startsWith('.-')) {
      meth = _.symbol.apply(null, op.toString().slice(2).split('/'));
      obj  = _.first(_.rest(exp));
      newExp = _.list(_.symbol('.-'), obj, meth);
      return evalPropertyAccessor(newExp, env);
    }
    else if ((sop = op.toString()).startsWith('.')) {
      meth = _.symbol.apply(null, op.toString().slice(1).split('/'));
      obj  = _.first(_.rest(exp));
      newExp = _.list(_.symbol('.'), obj, _.cons(meth, _.rest(_.rest(exp))));
      return evalMethodApplication(newExp, env);
    }
    else if (sop.endsWith('.')) {
      ctr = _.symbol.apply(null, sop.slice(0, sop.length - 1).split('/'));
      newExp = _.cons(_.symbol('new'), _.cons(ctr, _.rest(exp)));
      return evalClassInstantiation(newExp, env);
    }
    else if (_.isAssociative(op) || _.isKeyword(op) || _.isSet(op) || (isQuoted(op) && _.isSymbol(op_ = evalQuote(op)))) {
      return (op_ || op).apply(null, args);
    }

    func = ws.eval(op, env, callstack);

    if (isInvocable(func)) {
      return func.apply(null, args);
    }
    else if (func && func['-invoke']) {
      return func['-invoke'](args);
    }
    else {
      console.log('exp: ', ws.inspect(exp));
      console.log('env: ', env.toString());
      console.log('func value: ', func);
      throw new Error(["'", ws.inspect(op), "' is not a function"].join(''));
    }
  };

  var isBlock = ws.isBlock = makeTagPredicate(_.symbol('do'));

  // FIXME: module scoped vars defined within a block are not accessible within a block unless they are fully qualified
  var evalBlock = function(exp, env) {
    var body = _.rest(exp);
    var scope = env.extend();
    var value = null;
    _.each(body, function(exp) {
      value = ws.eval(exp, scope);
    });
    return value;
  };

  var isCond = ws.isCond = makeTagPredicate(_.symbol('cond'));

  var evalCond = function(exp, env) {
    var rest = _.rest(exp);
    if (_.count(rest) % 2 !== 0) throw new Error(['cond requires an even number of elements ', ws.inspect(exp)].join(''));
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

  var isMacroDef = makeTagPredicate(_.symbol('defmacro'));

  ws.TAGGED_SPECIAL_FORMS = _.set([
    _.symbol('fn'),
    _.symbol('quote'),
    _.symbol('cond'),
    _.symbol('def'),
    _.symbol('do'),
    _.symbol('try'),
    _.symbol('catch'),
    _.symbol('throw'),
    _.symbol('recur'),
    _.symbol('set!'),
    _.symbol('deftype'),
    _.symbol('defprotocol'),
    _.symbol('defmacro'),
    _.symbol('.-'),
    _.symbol('.'),
    _.symbol('new'),
    _.symbol('ns'),
    _.symbol('require'),
    _.symbol('loop'),
    _.symbol('use')
  ]);

  var evalMacroDef = function(exp, env) {
    var rest, name, args, body, lambda, fn, meta, forms;

    rest = _.rest(exp);
    name = _.first(rest);
    forms = _.rest(rest);
    var x = _.first(forms);
    if (_.isString(x) && _.isMap(_.second(forms))) {
      args = _.second(_.rest(forms));
      body = _.rest(_.rest(_.rest(forms)));
      meta = _.assoc(_.second(forms), _.keyword('doc'), x);
    }
    else if (_.isString(x)) {
      args = _.second(forms);
      body = _.rest(_.rest(forms));
      meta = _.hashMap(_.keyword('doc'), x);
    }
    else if (_.isMap(x)) {
      args = _.second(forms);
      body = _.rest(_.rest(forms));
      meta = x;
    }
    else {
      args = x;
      body = _.rest(forms);
      meta = _.hashMap();
    }

    lambda = _.cons(_.symbol('lambda'), _.cons(args, body));
    //ns = _.namespace(name);

    env.define(name); // so macro can refer to itself
    fn = evalLambda(lambda, env);
    fn.$lang$ws$ident = name; // give it an ident for stacktrace
    defineVariable(env, name, fn, _.assoc(meta, _.keyword('macro'), true));
    
    return name;
  };

  var getMacro = function(scope, name) {
    if (scope === null) return null;
    var env = ws.isEnv(scope) ? scope : scope["@@SCOPE@@"];
    if (env) {
      var obj;
      try {
        obj = env.getObject(_.name(name));
      }
      catch (e) {
        return null;
      }
      var meta = obj.getMeta();
      if (_.get(meta, _.keyword('macro')) === true) {
        return obj.get();
      }
    }
    else {
      //console.log(scope);
      //ws.pprint(name);
      //throw new Error('nothing in @@SCOPE@@');
    }
    return null;
  };

  var isMacro = ws.isMacro = ws['macro?'] = function(name) {
    var v;
    try {
      v = ws.lookupVar(name, globalEnv);
    }
    catch (e) {
      if (e instanceof zera.core.UndefinedVariableException) {
        return null;
      }
      else {
        throw e;
      }
    }
    var meta = v.getMeta();
    if (_.get(meta, _.keyword('macro')) === true) {
      return v.get();
    }
    return null;
  };

  var isTaggedList = ws['tagged-list?'] = function(exp, tag) {
    if (tag) {
      return _.isList(exp) && _.equals(_.first(exp), tag);
    }
    else {
      return _.isList(exp) && _.isSymbol(_.first(exp));
    }
  };

  var macroexpand = ws.macroexpand = function(exp) {
    var macro, name;
    if (!isTaggedList(exp)) {
      return exp;
    }
    name = _.first(exp);
    if (_.hasKey(ws.TAGGED_SPECIAL_FORMS, name)) {
      return exp;
    }
    var sname = ('' + name);
    if (sname.endsWith('.') || sname.startsWith('.')) {
      return exp;
    }
    if (macro = isMacro(name)) {
      return macroexpand(macro.apply(macro, _.intoArray(_.rest(exp))));
    }
    return exp;
  };

  var isAssignment = ws.isAssignment = makeTagPredicate(_.symbol('set!'));

  var evalAssignment = function(exp, env) {
    var name  = _.second(exp);
    var value = ws.eval(_.second(_.rest(exp)), env);
    ws.lookupVar(name, env).set(value);
    return value;
  };

  var isThrownException = ws.isThrownException = makeTagPredicate(_.symbol('throw'));

  var evalThrownException = function(exp, env) {
    var error = ws.eval(_.second(exp), env);
    if (_.isString(error)) {
      throw new Error(error); //wsError(error, env.stacktrace()));
    }
    else {
      throw error;
    }
  };

  var isPropertyAccessor = ws.isPropertyAccessor = makeTagPredicate(_.symbol('.-'));

  var evalPropertyAccessor = function(exp, env) {
    var obj = ws.eval(_.second(exp), env);
    if (obj == null) throw new Error(["nil is not an object from: ", ws.inspect(_.second(exp))].join(''));
    var prop = _.second(_.rest(exp));
    if (_.isSymbol(prop)) {
      return obj[prop];
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
      return obj[prop] = value;
    }
    else {
      return obj[ws.eval(prop, env)] = value;
    }
  };

  var isClassInstantiation = ws.isClassInstantiation = makeTagPredicate(_.symbol('new'));

  var evalClassInstantiation = ws.evalClassInstantiation = function(exp, env) {
    var ctr = ws.eval(_.second(exp), env);
    if (!_.isFunction(ctr)) throw new Error('class given is not a valid constructor');
    var args = _.intoArray(mori.map(function(arg) { return ws.eval(arg, env); }, _.rest(exp)));
    return new (ctr.bind.apply(ctr, args));
  };

  ws.Namespace = _.makeType(
    _.symbol('zera.core', 'Namespace'),
    _.hashMap(),
    function(name) {
      if (!_.isSymbol(name)) throw new Error("module name should be a symbol");
      this.name  = name;
      this.scope = globalEnv.extend();
      this.scope.define('*namespace*', this, _.hashMap(_.keyword('dynamic'), true));
    }
  );

  ws.Namespace.cache = {};
  ws.Namespace.intern = function(name) {
    var cache = ws.Namespace.cache;
    if (!cache[name]) {
      var ns = new ws.Namespace(name);
      //ns.importJSModule(ns.extern(ROOT_OBJECT));
      cache[name] = ns;
      globalEnv.define(name, ns, _.hashMap(_.keyword('tag'), _.symbol('zera.core', 'Namespace')));
    }
    return cache[name];
  };

  ws.Namespace.get = function(name) {
    var cache = ws.Namespace.cache;
    if (cache[name]) return cache[name];
    var obj = globalEnv.getObject(name);
    var meta = obj.getMeta();
    if (meta && _.get(meta, _.keyword('module')) === true) {
      cache[name] = obj.get();
      return cache[name];
    }
    throw new Error(_.str("'", name, "' doesn't seem to be a module"));
  };

  ws.Namespace.prototype.getScope = function() {
    return this.scope;
  };

  ws.Namespace.prototype.getVars = function() {
    return this.scope.getObjects();
  };

  ws.Namespace.prototype.getVar = function(name) {
    return this.scope.getObject(name);
  };

  ws.Namespace.prototype.lookup = function(name) {
    return this.scope.lookup(name);
  };

  ws.Namespace.prototype.fetchVar = function(name) {
    return this.scope.fetchObject(name);
  };

  ws.Namespace.prototype.getName = function() {
    return this.name;
  };

  ws.Namespace.prototype.extern = function(root) {
    var name = this.name;
    if (!_.isSymbol(name)) throw new Error('symbol expected');
    if (!root) throw new Error('root object is required');
    var path = ('' + name).split('/')[0].split('.');
    var mod = root;
    var name;
    for (var i = 0; i < path.length; i++) {
      name = path[i];
      if (!_.isObject(mod[name])) {
        mod[name] = {};
      }
      mod = mod[name];
    }
    if (!mod["@@SELF@@"]) mod["@@SELF@@"] = this;
    this.jsmod = mod;
    return mod;
  };

  ws.Namespace.prototype.define = function(name, value, meta) {
    //this.jsmod[name] = value;
    return this.scope.define(name, value, meta);
  };

  ws.Namespace.prototype.intern = function(name, value, meta) {
    var scope = this.scope.lookup(name);
    if (scope !== null) {
      return scope.getObject(name);
    }
    else {
      this.scope.define(name, value, meta);
      return this.scope.getObject(name);
    }
  };

  ws.Namespace.prototype.alias = function(alias, ns) {
    return this.define(alias, ns, _.hashMap(_.keyword('ns', 'alias'), true));
  };

  ws.Namespace.prototype.refer = function(ns, f) {
    var that = this;
    _.each(this.getVars(), function(x) {
      if (!f || ws.apply(f, x)) {
        that.define(_.name(_.nth(x, 0)), _.nth(x, 1), _.hashMap(_.keyword('ns', 'ref'), true));
      }
    });
    return this;
  };

  ws.Namespace.prototype.export = function(root) {
    var jsmod = this.extern(root);
    var vars = this.getVars();
    _.each(vars, function(v) {
      jsmod[_.nth(v, 0)] = _.nth(v, 1).get();
    });
    return true;
  };

  ws.Namespace.prototype.importJSModule = function(jsmod, ex) {
    if (jsmod === null) return null;
    else {
      var props = Object.getOwnPropertyNames(jsmod);
      var props, i;
      for (i = 0; i < props.length; i++) {
        var prop = props[i];
        if (prop === "@@SELF@@" || _.has(ex, prop)) continue;
        var name = _.name(zera.reader.readSymbol(prop));
        this.scope.define(name, jsmod[prop], _.hashMap(_.keyword('imported'), true));
      }
      return true;
    }
  };

  ws.Namespace.prototype.toString = function() {
    return _.str("#<Namespace name: ", this.name, ">");
  };

  ws['ns?'] = function(val) {
    if (val == null) return false;
    else {
      return val instanceof ws.Namespace;
    }
  };

  ws['ns-scope'] = function(mod) {
    return mod.getScope();
  };

  ws['ns-name'] = function(mod) {
    return mod.getName();
  };

  ws['ns-vars'] = function(mod) {
    return mod.getVars();
  };

  var defineModule = function(name) {
    var ns = ws.Namespace.intern(name);
    ws.NS_SCOPE = ns;
    return ns;
  };
  ws.defineModule = defineModule;

  var isModuleDefinition = ws.isModuleDefinition = makeTagPredicate(_.symbol('ns'));

  var evalModuleDefinition = function(exp, env) {
    var name = _.second(exp); 
    return defineModule(name);
  };

  var isModuleRequire = ws.isModuleRequire = makeTagPredicate(_.symbol('require'));

  var evalModuleRequire = function(exp, env) {
    var name = _.second(exp);
    var dir = typeof exports !== 'undefined' ? env.lookup('*dir*').get('*dir*') : '/';

    if (_.isSymbol(name)) {
      name = [('' + name).split('.').join('/'), ".ws"].join('');
    }

    if (_.isString(name)) {
      var full = dir ? [dir, name].join('/') : name;
      try {
        ws.readFile(full, env.extend());
      }
      catch (e) {
        throw new Error(["Reading file: \"", full, "\": ", e.message ? e.message : e].join(''));
      }
    }
    else {
      throw new Error("module name should be a symbol or string");
    }
    return true;
  };

  var isModuleSet = ws.isModuleSet = makeTagPredicate(_.symbol('use'));

  var evalModuleSet = function(exp, env) {
    var name = _.second(exp);
    if (_.isSymbol(name)) {
      var mod = evalVariable(name, globalEnv);
      ws.NS_SCOPE = mod;
    }
    else {
      throw new Error("namespace name should be a symbol");
    }
    return null;
  };

  var importModule = function(mod) {
    for (var fn in mod) {
      if (zera.util.hasOwnProperty(fn)) {
        globalEnv.define(fn, mod[fn]);
      }
    }
  };

  var isMethodApplication = ws.isMethodApplication = makeTagPredicate(_.symbol('.'));

  var evalMethodApplication = function(exp, env) {
    var args = _.rest(exp);
    var obj = ws.eval(_.first(args), env);
    if (obj == null) throw new Error(['nil is not an object from: ', ws.inspect(exp)].join(''));
    var method = _.second(args);
    if (_.isList(method)) {
      var mname = _.first(method);
      var m = obj[mname];
      if (m == null) {
        throw new Error(['method "', ws.inspect(mname), '" does not exist'].join(''));
      }
      return m.apply(obj, _.intoArray(mori.map(function(x) { return ws.eval(x, env); }, _.rest(method))));
    }
    else {
      var m = obj[method];
      if (m == null) {
        throw new Error(['method "', ws.inspect(method), '" does not exist'].join(''));
      }
      //ws.pprint(exp);
      //console.log('obj: ', obj);
      //console.log('meth: ', m);
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
    var scope = env.extend();

    if (!_.isVector(binds) || _.count(binds) !== 2) {
      console.log('binds', ws.inspect(binds));
      throw new Error("bindings should be a vector of 2 elements");
    }

    var vari = _.first(binds);
    var klass = ws.eval(_.second(binds), scope);

    if (e instanceof klass) {
      scope.define(vari, e);
      while ((expr = _.first(body)) !== null) {
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
    var catchBlock = mori.filter(isCatchBlock, body);
    var finallyBlock = _.first(mori.filter(isFinallyBlock, body));
    body = mori.remove(function(exp) { return isFinallyBlock(exp) || isCatchBlock(exp); }, body);
    var scope = env.extend();
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
          //ws.pprint(cBlock);
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

  var globalEnv = ws.env(); //.withMeta(_.hashMap(_.keyword('ident'), _.symbol('global'), _.keyword('source'), 'src/pbnj/wonderscript.js'));
  ws.DEFAULT_NS = defineModule(_.symbol('zera.core'));
  ws.DEFAULT_NS.importJSModule(zera.util);
  ws.NS_SCOPE = ws.DEFAULT_NS;
  ws.globalEnv = globalEnv;
  
  globalEnv.define('*source*', null, _.hashMap(_.keyword('dynamic'), true));

  var readerNS = defineModule(_.symbol('zera.reader'));
  readerNS.importJSModule(zera.reader);

  ws.DEFAULT_NS.define('macroexpand', macroexpand);
  ws.DEFAULT_NS.define('arity', arity);
  ws.DEFAULT_NS.define('var', ws['var']);

  ws.DEFAULT_NS.define('ns-name', ws['ns-name']);
  ws.DEFAULT_NS.define('ns-scope', ws['ns-scope']);
  ws.DEFAULT_NS.define('ns-vars', ws['ns-vars']);
  ws.DEFAULT_NS.define('ns?', ws['ns?']);
  
  ws.DEFAULT_NS.define('inspect', ws.inspect);

  ws.pprint = function(exp) {
    console.log(ws.inspect(exp));
  };
    
  ws.DEFAULT_NS.define('pprint', ws.pprint);
  ws.DEFAULT_NS.define('p', ws.pprint);

  ws.RESTORE = {frames: []};
  ws.RESTORE.popFrame = function() {
    return this.frames.pop();
  };

  var STACK_ID = 0;
  ws.stack = function() {
    var id = STACK_ID++;
    var ref = new _.Atom(_.list(), _.hashMap(), _.isList);
    /*ref.addWatch(_.str('STACK ', id, ' update'), function(key, ref, old, knew) {
      console.log(_.str(key, ":"), ws.inspect(old), ws.inspect(knew));
    });*/
    return ref;
  };

  ws.eval = function(exp, env, callstack) {
    var env = env || globalEnv;
    var callstack = callstack || ws.stack();
    var exp = macroexpand(exp);

    try {
      var out = null;
      if (ws.isSelfEvaluating(exp)) {
        out = exp;
      }
      else if (_.isKeyword(exp)) {
        out = evalKeyword(exp, env);
      }
      else if (isCollectionLiteral(exp)) {
        out = evalCollectionLiteral(exp, env);
      }
      else if (isVariable(exp)) {
        out = evalVariable(exp, env);
      }
      else if (isQuoted(exp)) {
        out = evalQuote(exp);
      }
      else if (isDefinition(exp)) {
        out = evalDefinition(exp, env);
      }
      else if (isCond(exp)) {
        out = evalCond(exp, env);
      }
      else if (isLambda(exp)) {
        out = evalLambda(exp, env, callstack);
      }
      else if (isBlock(exp)) {
        out = evalBlock(exp, env);
      }
      else if (isTryBlock(exp)) {
        out = evalTryBlock(exp, env);
      }
      else if (isLoop(exp)) {
        out = evalLoop(exp, env);
      }
      else if (isRecursionPoint(exp)) {
        out = evalRecursionPoint(exp, env);
      }
      else if (isAssignment(exp)) {
        out = evalAssignment(exp, env);
      }
      else if (isProtocol(exp)) {
        out = evalProtocol(exp, env);
      }
      else if (isType(exp)) {
        out = evalType(exp, env);
      }
      else if (isMacroDef(exp)) {
        out = evalMacroDef(exp, env);
      }
      else if (isThrownException(exp)) {
        out = evalThrownException(exp, env);
      }
      else if (isPropertyAccessor(exp)) {
        out = evalPropertyAccessor(exp, env);
      }
      else if (isPropertyAssignment(exp)) {
        out = evalPropertyAssignment(exp, env);
      }
      else if (isMethodApplication(exp)) {
        out = evalMethodApplication(exp, env);
      }
      else if (isClassInstantiation(exp)) {
        out = evalClassInstantiation(exp, env);
      }
      else if (isModuleDefinition(exp)) {
        out = evalModuleDefinition(exp, env);
      }
      else if (isModuleRequire(exp)) {
        out = evalModuleRequire(exp, env);
      }
      else if (isModuleSet(exp)) {
        out = evalModuleSet(exp, env);
      }
      else if (isApplication(exp)) {
        out = evalApplication(exp, env, callstack);
      }
      else {
        throw new Error(["invalid expression: '", exp, "'"].join(''));
      }
      return out;
    }
    catch (e) {
      if (e instanceof RecursionPoint) {
        throw e;
      }
      else {
        if (_.isEmpty(callstack.deref())) {
          callstack.swap(_.conj, trace(null));
        }
        console.log('callstack: ', ws.inspect(callstack.deref()));
        throw e;
      }
    }
  };
  
  ws.DEFAULT_NS.define('eval', ws.eval);
  ws.DEFAULT_NS.define('apply', ws.apply);

  var wsError = function(e, trace) {
    var wsStack = ''; //_.str(ws.pprint(exp), ' @ ', stream.source(), ':', stream.line(), ':', stream.column(), '\n');
    if (!_.isEmpty(trace)) {
      wsStack = fmtStacktrace(trace);
    }
    return [e.message ? e.message : e, ":\n", wsStack, e.stack ? _.str("\n", e.stack) : null].join('');
  };

  function fmtStacktrace(trace) {
    var i, x, buffer = [];
    for (i = 0; i < trace.length; i++) {
      x = trace[i];
      buffer.push([x[1], "@", x[0] || 'unknown', ":", x[2]].join(''));
    }
    return buffer.join('\n');
  }

  ws.withLocation = function(meta, source, line, column) {
    return _.assoc(
      meta,
      _.keyword('source'), source,
      _.keyword('line'), line,
      _.keyword('column'), column
    );
  };

  ws.readStream = function(stream, env) {
    var env = (env || globalEnv).extend(); //.varyMeta(withLocation(stream.source(), stream.line(), stream.column()));
    var callstack = ws.stack();
    try {
      var value = null;
      while (!stream.eof()) {
        var exp = stream.peek();
        value = ws.eval(exp, env, callstack);
        //ws.STREAM_META.swap(ws.withLocation, stream.source(), stream.line(), stream.column());
        stream.next();
      }
      return value;
    }
    catch(e) {
      console.error(e);
      //console.log('callstack: ', ws.inspect(env.callstack()));
      //console.log(env);
      //console.error(fmtStacktrace(env.stacktrace()));
      //throw e; //new Error(wsError(e, env.stacktrace()));
    }
  };
  
  ws.DEFAULT_NS.define('read-stream', ws.readStream);

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
  
  ws.DEFAULT_NS.define('compile-stream', ws.compileStream);

  ws.readString = function(str, input) {
    //console.log(str);
    var stream = zera.reader.readString(str, input);
    return ws.readStream(stream);
  };

  ws.readJS = function(str) {
    return ws.eval(zera.reader.readJS(str));
  };
  
  ws.DEFAULT_NS.define('read-string', ws.readString);

  ws.evalJS = function(exp) {
    //console.log(str);
    var data = zera.reader.readJS(exp);
    return ws.eval(data);
  };

  ws.DEFAULT_NS.define('eval-js', ws.evalJS);

  ws.compileString = function(str, input) {
    var stream = zera.reader.readString(str, input);
    return ws.compileStream(stream);
  };

  ws.DEFAULT_NS.define('compile-string', ws.compileString);

  ws.readFile = function(file, env) {
    var env = env || globalEnv;
    var scope = env.extend();
    scope.define('*file*', file);
    var path = file.split('/');
    scope.define('*dir*', path.slice(0, path.length - 1).join('/'));
    var stream = zera.reader.readFile(file);
    return ws.readStream(stream, scope);
  };

  ws.DEFAULT_NS.define('read-file', ws.readFile);

  ws.compileFile = function(file) {
    var stream = zera.reader.readFile(file);
    return ws.compileStream(stream);
  };

  ws.DEFAULT_NS.define('compile-file', ws.compileFile);
  
  globalEnv.define('*version*', '0.0.1-alpha');
  globalEnv.define('*mode*', _.keyword('production'), _.hashMap(_.keyword('dynamic'), true));
  globalEnv.define('*target-language*', _.keyword('javascript'), _.hashMap(_.keyword('dynamic'), true));

  ws.DEFAULT_NS.importJSModule(ws);

  function symbolImporter(mod) {
    return function(name) {
      if (typeof ROOT_OBJECT[name] !== 'undefined') {
        mod.define(name, eval(name), _.hashMap(_.keyword('imported'), true));
      }
    };
  }

  var js = defineModule(_.symbol('js'));
  [
    'Array',
    'ArrayBuffer',
    'AsyncFunction',
    'Atomics',
    'Boolean',
    'DataView',
    'Date',
    'Error',
    'EvalError',
    'Float32Array',
    'Float64Array',
    'Function',
    'Generator',
    'GeneratorFunction',
    'Infinity',
    'Int32Array',
    'Int64Array',
    'Int8Array',
    'InternalError',
    'Intl',
    'JSON',
    'Map',
    'Math',
    'NaN',
    'Number',
    'Object',
    'Promise',
    'Proxy',
    'RangeError',
    'ReferenceError',
    'Reflect',
    'RegExp',
    'Set',
    'String',
    'Symbol',
    'SyntaxError',
    'TypeError',
    'TypedArray',
    'URIError',
    'Uint16Array',
    'Uint32Array',
    'Uint8Array',
    'Uint8ClampedArray',
    'WeakMap',
    'WeakSet',
    'decodeURI',
    'decodeURIComponent',
    'encodeURI',
    'encodeURIComponent',
    'eval',
    'isFinite',
    'isNaN',
    'parseFloat',
    'parseInt',
    'uneval',
    'SIMD',
    'console'
  ].forEach(symbolImporter(js));

  // TODO: it'd be interesting to figure out how to give this the local scope
  function jsFunction(args) {
    var args_ = _.intoArray(args);
    var body = Array.prototype.slice.call(arguments, 1).reverse();
    var scope = globalEnv.extend(); //.setIdent('jsFunction');
    return function() {
      scope.define(_.symbol('this'), this);
      scope.define(_.symbol('arguments'), arguments);
      var i;
      for (i = 0; i < args_.length; i++) {
        scope.define(args_[i], arguments[i]);
      }
      var value;
      for (i = 0; i < body.length; i++) {
        value = ws.eval(body[i], scope);
      }
      return value;
    };
  }

  js.define(_.symbol('null'), null);
  js.define(_.symbol('undefined'), undefined);
  js.define(_.symbol('function'), jsFunction);
  js.define(_.symbol('null?'), _.isNull);
  js.define(_.symbol('undefined?'), _.isUndefined);
  js.define(_.symbol('number?'), _.isNumber);
  js.define(_.symbol('boolean?'), _.isBoolean);
  js.define(_.symbol('string?'), _.isString);
  js.define(_.symbol('date?'), _.isDate);
  js.define(_.symbol('error?'), _.isError);
  js.define(_.symbol('array?'), _.isArray);
  js.define(_.symbol('object?'), _.isObject);
  js.define(_.symbol('regexp?'), _.isRegExp);
  js.define(_.symbol('function?'), _.isFunction);
  js.define(_.symbol('arguments?'), _.isArguments);
  js.define(_.symbol('element?'), _.isElement);
  js.define(_.symbol('identical?'), function(a, b) { return a === b; });
  js.define(_.symbol('equiv?'), function(a, b) { return a == b; });
  js.define(_.symbol('array'), function() { return Array.prototype.slice.call(arguments); });
  js.define(_.symbol('instanceof?'), function(a, b) { return a instanceof b; });
  js.define(_.symbol('typeof'), function(x) { return typeof x; });

  // Node
  if (isNode) {
    globalEnv.define('*platform*', _.keyword('nodejs'));
    globalEnv.define('*platform-version*', _.str("Node.js ", process.version));

    module.exports = ws;
    var node = defineModule(_.symbol('js.node'));
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
      'process',
      'setImmediate',
      'setInterval',
      'setTimeout'
    ].forEach(symbolImporter(node));
    node.define(_.symbol('require'), require);
    node.define(_.symbol('module'), module);
    node.define(_.symbol('exports'), exports);
  }

  // Apple JSA 
  if (isJSA) {
    globalEnv.define('*platform*', _.keyword('jsa'));

    var jsa = defineModule(_.symbol('js.jsa'));
    [
      'Automation',
      'Application',
      'Library',
      'Path',
      'Progress',
      'ObjectSpecifier',
      'delay',
      'ObjC',
      'Rerf',
      '$'
    ].forEach(symbolImporter(jsa));
  }

  if (isBrowser) {
    globalEnv.define('*platform*', _.keyword('browser'));
    globalEnv.define('*platform-version*', window.navigator.userAgent);

    var browser = defineModule(_.symbol('js.browser'));
    [
      'WebAssembly',
      'window',
      'document',
      'location',
      'localStorage'
    ].forEach(symbolImporter(browser));

    var dom = defineModule(_.symbol('js.dom'));
    [
      'Attr',
      'ByteString',
      'CDATASection',
      'CharacterData',
      'ChildNode',
      'CSSPrimitiveValue',
      'CSSValue',
      'CSSValueList',
      'Comment',
      'CustomEvent',
      'Document',
      'DocumentFragment',
      'DocumentType',
      'DOMError',
      'DOMException',
      'DOMImplmentation',
      'DOMString',
      'DOMTimeStamp',
      'DOMStringList',
      'DOMTokenList',
      'Element',
      'Event',
      'EventTarget',
      'MutationObserver',
      'MutationRecord',
      'Node',
      'NodeFilter',
      'NodeIterator',
      'NodeList',
      'ParentNode',
      'ProcessingInstruction',
      'Range',
      'Text',
      'TreeWalker',
      'URL',
      'Window',
      'Worker',
      'XMLDocument',
      'HTMLAnchorElement',
      'HTMLAreaElement',
      'HTMLAudioElement',
      'HTMLBaseElement',
      'HTMLBodyElement',
      'HTMLBREElement',
      'HTMLButtonElement',
      'HTMLCanvasElement',
      'HTMLDataElement',
      'HTMLDataListElement',
      'HTMLDialogElement',
      'HTMLDivElement',
      'HTMLDListElement',
      'HTMLEmbedElement',
      'HTMLFieldSetElement',
      'HTMLFontElement',
      'HTMLFormElement',
      'HTMLFrameSetElement',
      'HTMLHeadElement',
      'HTMLHtmlElement',
      'HTMLHRElement',
      'HTMLIFrameElement',
      'HTMLImageElement',
      'HTMLInputElement',
      'HTMLKeygenElement',
      'HTMLLabelElement',
      'HTMLLIElement',
      'HTMLLinkElement',
      'HTMLMapElement',
      'HTMLMediaElement',
      'HTMLMetaElement',
      'HTMLMeterElement',
      'HTMLModElement',
      'HTMLObjectElement',
      'HTMLOListElement',
      'HTMLOptGroupElement',
      'HTMLOptionElement',
      'HTMLOutputElement',
      'HTMLParagraphElement',
      'HTMLParamElement',
      'HTMLPreElement',
      'HTMLProgressElement',
      'HTMLQuoteElement',
      'HTMLScriptElement',
      'HTMLSelectElement',
      'HTMLSourceElement',
      'HTMLSpanElement',
      'HTMLStyleElement',
      'HTMLTableElement',
      'HTMLTableCaptionElement',
      'HTMLTableCellElement',
      'HTMLTableDataCellElement',
      'HTMLTableHeaderCellElement',
      'HTMLTableColElement',
      'HTMLTableRowElement',
      'HTMLTableSectionElement',
      'HTMLTextAreaElement',
      'HTMLTimeElement',
      'HTMLTitleElement',
      'HTMLTrackElement',
      'HTMLUListElement',
      'HTMLUnknownElement',
      'HTMLVideoElement',
      'CanvasRenderingContext2D',
      'CanvasGradient',
      'CanvasPattern',
      'TextMetrics',
      'ImageData',
      'CanvasPixelArray',
      'NotifyAudioAvailableEvent',
      'HTMLFormControlsCollection',
      'HTMLOptionsCollection',
      'DOMStringMap',
      'RadioNodeList',
      'MediaError'
    ].forEach(symbolImporter(dom));

    // read core library
    ws.readString(CORE_ZERA); // src/zera/core.zera
    ws.readString(JS_ZERA); // src/zera/js.zera
    ws.readString(HTML_ZERA); // src/zera/core/html.zera

    var scripts = document.getElementsByTagName('script');
    var wscode  = [];
    for (var i = 0; i < scripts.length; i++) {
      if (scripts[i].type == "text/zera") {
        wscode.push(scripts[i].text);
      }
    }
    wscode.forEach(function(code) {
      ws.readString(code);
    });
  }
  defineModule(_.symbol('user'));
}()); // namespace zera.core