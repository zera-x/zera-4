namespace pbnj.core {

  var _;
  if (typeof exports !== 'undefined') {
    _ = require('./core.js');
  }
  else {
    _ = pbnj.core;
  }

  function Variable(name, value, meta) {
    this.$name = name;
    this.$value = value;
    this.$meta = meta;
  }

  var Var = Variable.prototype;

  Var.getMeta = function() {
    return this.$meta;
  };

  Var.setValue = function(value) {
    this.$value = value;
    return this;
  };

  Var.getValue = function() {
    return this.$value;
  };

  Var.deref = Var.getValue;

  Var.getName = function() {
    return this.$value;
  };

  Var.withMeta = function(meta) {
    this.$meta = meta;
    return this;
  };

  Var.varyMeta = function(f, args) {
    this.$meta = f.apply(null, [this.$meta].concat(_.intoArray(args)));
    return this;
  };

  Var.toString = function() {
    return _.str('#<Variable name: ', this.$name, ' meta: ', this.$meta, ' value: ', this.$value, '>');
  };

  var id = 0;

  /**
   * @constructor
   * @final
   */
  function Env(parent) {
    this.vars = Object.create(parent ? parent.vars : null);
    this.parent = parent;
    this.define('*scope*', this);
    if (!parent) {
      this.define('*source*', 'unknown');
      this.define('*scope-name*', 'global');
      this.define('*line*', 1);
      this.trace = ['unknown', 'global', 1];
    }
    else {
      this.trace = [this.parent.get('*source*'),
                    this.parent.get('*scope-name*'),
                    this.parent.get('*line*')];
    }
    this.id = id++;
  }

  var env = Env.prototype;

  env.extend = function() {
    return new Env(this);
  };

  env.lookup = function(name) {
    var scope = this;
    while (scope) {
      if (Object.prototype.hasOwnProperty.call(scope.vars, name)) {
        return scope;
      }
      scope = scope.parent;
    }
    return null;
  };

  env.getObject = function(name) {
    if (name in this.vars) {
      return this.vars[name];
    }
    throw new Error(_.str("Undefined variable: '", name, "'"));
  };

  env.getObjects = function() {
    var buffer = [];
    var scope = this;
    while (scope) {
      for (var prop in scope.vars) {
        if (Object.prototype.hasOwnProperty.call(scope.vars, prop)) {
          buffer.push(scope.vars[prop]);
        }
      }
      scope = scope.parent;
    }
    return mori.list.apply(null, buffer);
  };

  env.get = function(name) {
    var obj = this.getObject(name);
    return obj.getValue();
  };

  env.set = function(name, value) {
    var scope = this.lookup(name);
    // cannot define globals from a nested environment
    if (!scope && this.parent) throw new Error(_.str("Undefined variable: '", name, "'"));
    (scope || this).vars[name].setValue(value);
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

  env.define = function(name, value, meta) {
    var sname = _.str(name);
    var meta = _.merge(_.hashMap(_.keyword('name'), _.symbol(sname), _.keyword('tag'), typeTag(value)), meta || _.hashMap());

    var scope, m = _.hashMap();
    if (scope = this.lookup('*scope-name*')) {
      m = _.assoc(m, _.keyword('scope-name'), scope.get('*scope-name*'));
    }
    if (scope = this.lookup('*line*')) {
      m = _.assoc(m, _.keyword('line'), scope.get('*line*'));
    }
    if (scope = this.lookup('*column*')) {
      m = _.assoc(m, _.keyword('column'), scope.get('*column*'));
    }
    if (scope = this.lookup('*source*')) {
      m = _.assoc(m, _.keyword('source'), scope.get('*source*'));
    }

    if (_.isFunction(value) && value.arglists) {
      m = _.assoc(m, _.keyword('arglists'), value.arglists());
    }

    this.vars[sname] = new Variable(_.symbol(sname), value, _.merge(m, meta));
    return value;
  };

  env.setLocation = function(line, column) {
    this.define('*line*', line);
    this.define('*column*', column);
    return this;
  };

  env.setSource = function(source) {
    this.define('*source*', source);
    return this;
  };

  env.setIdent = function(ident) {
    this.define('*scope-name*', _.symbol(_.str(ident)));
    return this;
  };

  env.getIdent = function() {
    try {
      return this.lookup('*scope-name*').get('*scope-name*');
    }
    catch (e) {
      return null;
    }
  };

  env.stash = function() {
    var scope = this.lookup('*stash*');
    if (scope) {
      return scope.get('*stash*');
    }
    return null;
  };

  env.initStash = function() {
    var stash = new Env();
    this.define('*stash*', stash);
    return this;
  };

  env.setStashValue = function(key, value) {
    var stash = this.stash();
    if (stash) {
      stash.define(key, value);
    }
    else {
      throw new Error("could not find stash");
    }
    return this;
  };

  env.getStashValue = function(key) {
    var stash = this.stash();
    if (stash) {
      try {
        return stash.get(key);
      }
      catch (e) {
        return null;
      }
    }
    return null;
  };

  env.stacktrace = function() {
    var trace = [];
    var scope = this;
    while (scope) {
      trace.unshift(scope.trace);
      scope = scope.parent;
    }
    return trace;
  };

  pbnj.env = function(parent) {
    return new Env(parent);
  };

  pbnj.variable = function(name, value, meta) {
    return new Variable(name, value, meta);
  };

  pbnj.core.isEnv = function(val) {
    return val instanceof Env;
  };

  if (typeof exports !== 'undefined') {
    module.exports.env = pbnj.env;
    module.exports.isEnv = pbnj.core.isEnv;
    module.exports.variable = pbnj.variable;
  }
}
