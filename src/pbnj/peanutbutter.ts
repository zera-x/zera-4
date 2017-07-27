namespace pbnj.peanutbutter {

  var _, ROOT_OBJECT;
  if (typeof exports !== 'undefined') {
    mori = require('mori');
    _    = require('./core.js');
    var pbnj = pbnj || {};
    pbnj.core = _;
    pbnj.reader = require('./reader.js');
    pbnj.wonderscript = require('./wonderscript.js');
    ROOT_OBJECT = global;
  }
  else {
    ROOT_OBJECT = window;
    _ = ROOT_OBJECT.pbnj.core;
  }

  pbnj.wonderscript.readFile("src/pbnj/core.ws");
  pbnj.wonderscript.readFile("src/pbnj/peanutbutter.ws");

  var pb = ROOT_OBJECT.pbnj.peanutbutter;

  pb.compileFile = function(file) {
    return pb.compileStream(pbnj.reader.readFile(file));
  };

  pb.compileString = function(input, source) {
    return pb.compileStream(pbnj.reader.readString(input, source));
  };

  pb.compileStream = function(stream) {
    var buffer = [];
    while (!stream.eof()) {
      buffer.push(pb.compile.call(null, stream.next()));
    }
    return buffer.join('\n');
  };

  pb.compileJS = function(exps) {
    var value = null;
    _.each(exps, function(exp) {
      value = pb.compile(pbnj.reader.readJS(exp));
    });
    return value;
  };

  if (typeof exports !== 'undefined') {
    module.exports = ROOT_OBJECT.pbnj.peanutbutter;
  }
}

