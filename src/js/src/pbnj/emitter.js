goog.provide('pbnj.emitter');

goog.require('pbnj.core');

goog.scope(function() {
  /** @const */
  var _ = pbnj.core;

  /** @interface */
  pbnj.IEmitter = function() {};
  pbnj.IEmitter.prototype.selfEvaluting = function(exp) {};
  pbnj.IEmitter.prototype.variable = function(exp) {};
  pbnj.IEmitter.prototype.quote = function(exp) {};
  pbnj.IEmitter.prototype.definition = function(exp) {};
  pbnj.IEmitter.prototype.cond = function(exp) {};
  pbnj.IEmitter.prototype.lambda = function(exp) {};
  pbnj.IEmitter.prototype.application = function(exp) {};

  /** @implments {pbnj.IEmitter} */
  function JSEmitter(compile) {
    if (!compile) throw new Error('compile function is required');
    this.compile = compile
  }
  var js = JSEmitter.prototype;
  
  js.selfEvaluating = function(exp) {
    if (_.isNull(exp)) return 'null';
    else if (_.isNumber(exp)) return _.str("(", exp, ")");
    else if (_.isBoolean(exp)) return exp === true ? '(true)' : '(false)';
    else if (_.isString(exp)) return JSON.stringify(exp);
    else if (_.isKeyword(exp)) return _.str("pbnj.core.keyword('", exp.toString().replace(/^:/, ''), "')");
    else if (_.isDate(exp)) return _.str("(new Date(", exp.valueOf(), "))");
    else if (_.isRegExp(exp)) {
      var flags = [];
      if ( exp.ignoreCase ) flags.push('i');
      if ( exp.global ) flags.push('g');
      if ( exp.multiline ) flags.push('m');
      return str('(new RegExp("', exp.source, '", "', flags.join(''), '"))');
    }
    else if (_.isMap(exp)) {
      return _.str("pbnj.core.hashMap(", _.intoArray(_.map(_.concat.apply(null, _.toArray(exp)), js.selfEvaluating)).join(","), ")");
    }
    else if (_.isCollection(exp)) {
      var delims = _.isVector(exp) ? ['pbnj.core.vector(', ')'] : ['pbnj.core.set(', ')'];
      return _.str(delims[0], _.intoArray(_.map(exp, js.selfEvaluating)).join(","), delims[1]);
    }
    else {
      throw new Error(_.str("invalid expression: ", exp));
    }
  };

  js.variable = function(exp, env) {
    return _.str(exp);
  };

  // TODO: complete this!
  js.quote = function(exp) {
    return _.second(exp);
  };

  js.definition = function(exp, env) {
    var rest = _.rest(exp);
    var name = _.first(rest);
    var value = _.second(rest);
    if (!name) throw new Error("a name is required");
    else if (!value) return _.str('var ', name, ';');
    return _.str('var ', _.first(rest), " = ",  this.compile(_.second(rest)), ';')
  };

  js.cond = function(exp, env) {
    var compile = this.compile;
    var rest = _.intoArray(_.rest(exp));
    var pairs = _.pair(rest);
    var conds = _.map(pairs,
        function (pair, i) {
          if (i === 0) return _.str('if (', compile(pair[0]), ') { return ', compile(pair[1]), ' }');
          else if (_.equals(pair[0], _.keyword('else'))) return _.str(' else { return ', compile(pair[1]), ' }');
          else {
            return _.str(' else if (', compile(pair[0]), ') { return ', compile(pair[1]), ' }');
          }
        });
    return _.str('(function(){ ', conds.join(''), '}())');
  };

  js.lambda = function(exp, env) {
    var rest = _.rest(exp);
    var names = _.intoArray(_.first(rest));
    var body = _.intoArray(_.rest(rest));
    var compile = this.compile;
    return _.str('(function(', _.map(names,
                                  function(name) {
                                    if (!_.isSymbol(name)) throw new Error("symbol expected");
                                    return _.str(name) }).join(', '), ') { ', _.map(body,
                                                                               function(exp, i) {
                                                                                 if (i === body.length - 1) return _.str('return ', compile(exp), ';')
                                                                                 return _.str(compile(exp), ';'); }), '})');
                                                                                
  };

  js.application = function(exp, env) {
    var args = _.intoArray(_.rest(exp));
    return _.str(this.compile(_.first(exp)), '(', _.map(args, this.compile.bind(this)).join(', '), ')');
  };

  pbnj.emitter.js = function(compile) {
    return new JSEmitter(compile);
  };

});
