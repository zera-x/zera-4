namespace pbnj.peanutbutter {

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

  export function compileFile(file) {
    return compileStream(pbnj.reader.readFile(file));
  }

  export function compileString(input, source) {
    return compileStream(pbnj.reader.readString(input, source));
  }

  export function compileStream(stream) {
    var buffer = [];
    while (!stream.eof()) {
      buffer.push(pb.compile.call(null, stream.next()));
    }
    return buffer.join('\n');
  }

  export function compileJS(exps) {
    var value = null;
    _.each(exps, function(exp) {
      value = pb.compile(pbnj.reader.readJS(exp));
    });
    return value;
  }

  if (typeof exports !== 'undefined') {
    module.exports = _.merge(peanutbutter, ROOT_OBJECT.pbnj.peanutbutter);
  }
}

