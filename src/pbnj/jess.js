goog.provide('pbnj.jess');

goog.require('pbnj.reader');
goog.require('pbnj.core');

goog.scope(function() {

  var jess = pbnj.jess;
  var _ = pbnj.core;
  var str = pbnj.core.str;
  var pair = pbnj.core.pair;
  var map = pbnj.core.map;
  var pluck = pbnj.core.pluck;
  var assoc = pbnj.core.assoc;
  
  var pprint = function(form) {
    if ( typeof form === 'string' ) return str('"', form, '"');
    else if ( typeof form === 'boolean' ) return form;
    else if ( form === null ) return 'null';
    else if ( typeof form === 'undefined' ) return 'undefined';
    else if ( form instanceof Date ) return str('new Date(', form.valueOf(), ')');
    else if ( form instanceof RegExp ) {
      var flags = [];
      if ( form.ignoreCase ) flags.push('i');
      if ( form.global ) flags.push('g');
      if ( form.multiline ) flags.push('m');
      return str('/', form.source, '/', flags.join(''));
    }
    else if ( form instanceof Array ) {
      var delim = form.type === 'list' ? ['(', ')'] : ['[', ']']
        , sep = form.type === 'list' ? ' ' : ', ';
      return str(delim[0], map(form, function(x){ return pprint(x) }).join(sep), delim[1]);
    }
    else {
      return "" + form;
    }
  };

  jess.pprint = pprint;

  jess.readFile = function(file) {
    return jess.readStream(pbnj.reader.readFile(file));
  };

  jess.readString = function(input, source) {
    return jess.readStream(pbnj.reader.readString(input, source));
  };

  jess.readStream = function(stream) {
    var value = null;
    while (!stream.eof()) {
      value = eval(jess.compile(stream.next()));
    }
    return value;
  };

  jess.compileFile = function(file) {
    return jess.compileStream(pbnj.reader.readFile(file));
  };

  jess.compileString = function(input, source) {
    return jess.compileStream(pbnj.reader.readString(input, source));
  };

  jess.compileStream = function(stream) {
    var buffer = [];
    while (!stream.eof()) {
      buffer.push(jess.compile(stream.next()));
    }
    return buffer.join('\n');
  };

  jess.compileJS = function(exps) {
    var value = null;
    _.each(exps, function(exp) {
      value = jess.compile(pbnj.reader.readJS(exp));
    });
    return value;
  };
});
