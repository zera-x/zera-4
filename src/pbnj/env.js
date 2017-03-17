goog.provide('pbnj.env');

goog.require('pbnj.core');

goog.scope(function() {
  var _ = pbnj.core;

  function WSObject(value, meta) {
    this.$value = value;
    this.$meta = meta;
  }

  var wso = WSObject.prototype;

  wso.getMeta = function() {
    return this.$meta;
  };

  wso.setValue = function(value) {
    this.$value = value;
    return this;
  };

  wso.getValue = function() {
    return this.$value;
  };

  wso.withMeta = function(meta) {
    this.$meta = meta;
    return this;
  };

  wso.varyMeta = function(f, args) {
    this.$meta = f.apply(null, [this.$meta].concat(_.intoArray(args)));
    return this;
  };

  wso.toString = function() {
    return _.str('#<WSObject meta: ', this.$meta, ' value: ', this.$value, '>');
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
      var nm = value.constructor.name;
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
    this.vars[sname] = new WSObject(value, _.merge(m, meta));
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

  env.stacktrace = function() {
    var trace = [];
    var scope = this;
    while (scope) {
      trace.unshift(scope.trace);
      scope = scope.parent;
    }
    return trace;
  };

  /**
   * @export
   * @nocollapse
   */
  pbnj.core.Env = Env;

  pbnj.env = function(parent) {
    return new pbnj.core.Env(parent);
  };

  pbnj.core.isEnv = function(val) {
    return val instanceof pbnj.core.Env;
  };

  if (module != void 0 && module.exports) {
    module.exports.env = pbnj.env;
  }
});
