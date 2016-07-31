goog.provide('pbnj.env');

goog.require('pbnj.core');

goog.scope(function() {
  var _ = pbnj.core;

  function Env(parent) {
    this.vars = Object.create(parent ? parent.vars : null);
    this.parent = parent;
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
    throw new Error(_.str("Undefined variable: ", name));
  };

  env.get = function(name) {
    if (name in this.vars) {
      return this.vars[name]
    }
    throw new Error(_.str("Undefined variable: ", name));
  };

  env.set = function(name, value) {
    var scope = this.lookup(name);
    // cannot define globals from a nested environment
    if (!scope && this.parent) throw new Error(_.str("Undefined variable", name));
    return (scope || this).vars[name] = value;
  };

  env.define = function(name, value) {
    return this.vars[name] = value;
  };

  pbnj.env = function(parent) {
    return new Env(parent);
  };

  if (module != void 0 && module.exports) {
    module.exports.env = pbnj.env;
  }
});
