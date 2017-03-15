goog.provide('pbnj.env');

goog.require('pbnj.core');

goog.scope(function() {
  var _ = pbnj.core;

  var level = 0;

  function Env(parent) {
    this.vars = Object.create(parent ? parent.vars : null);
    this.parent = parent;
    if (!parent) {
      this.vars['*source*'] = 'unknown';
      this.vars['*scope-name*'] = 'global';
      this.vars['*line*'] = 1;
      this.trace = ['unknown', 'global', 1];
    }
    else {
      this.trace = [this.parent.vars['*source*'],
                    this.parent.vars['*scope-name*'],
                    this.parent.vars['*line*']];
    }
    this.level = level++;
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

  env.get = function(name) {
    if (name in this.vars) {
      return this.vars[name];
    }
    throw new Error(_.str("Undefined variable: '", name, "'"));
  };

  env.set = function(name, value) {
    var scope = this.lookup(name);
    // cannot define globals from a nested environment
    if (!scope && this.parent) throw new Error(_.str("Undefined variable: '", name, "'"));
    (scope || this).vars[name] = value;
    return value;
  };

  env.define = function(name, value) {
    this.vars[name] = value;
    return value;
  };

  env.setLocation = function(line, column) {
    this.line = line;
    this.column = column;
    this.vars['*line*'] = line;
    this.vars['*column*'] = column;
    return this;
  };

  var getter = function(dim) {
    return function() {
      var scope = this;
      while (scope) {
        if (scope[dim]) return scope[dim];
        scope = scope.parent;
      }
      return null;
    };
  };

  env.setSource = function(source) {
    this.source = source;
    this.vars['*source*'] = source;
    return this;
  };

  env.setIdent = function(ident) {
    this.ident = ident;
    this.vars['*scope-name*'] = ident;
    return this;
  };

  env.getLine = getter('line');
  env.getColumn = getter('column');
  env.getSource = getter('source');
  env.getIdent = getter('ident');

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

  pbnj.core.isEnv = function(val) {
    return val instanceof Env;
  };

  if (module != void 0 && module.exports) {
    module.exports.env = pbnj.env;
  }
});