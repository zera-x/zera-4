namespace zera.reader {

  var _, pair, mori;
  if (typeof exports !== 'undefined') {
    mori = require('mori');
    _ = require('./util.js');
    pair = _.pair;
  }
  else {
    mori = window.mori;
    pair = zera.util.pair;
    _ = zera.util;
  }

  // should support full EDN spec
  // see: https://github.com/edn-format/edn
  //
  // also look to the Clojure reader spec for ideas
  // see: http://clojure.org/reference/reader 

  // TODO: add support for dispatching for #(), #{}, #inst, #"", etc.

  var STREAM_META = new _.Atom(_.hashMap(), _.hashMap(), _.isMap);
  /*STREAM_META.addWatch('STREAM_META update', function(key, ref, old, knew) {
    console.log(_.str(key, ":"), _.inspect(old), _.inspect(knew));
  });*/


  function inputStream(input, sourceName) {
    STREAM_META.swap(_.assoc, _.keyword('source'), sourceName);
    var pos = 0, line = 1, col = 0;
    return {
      next: next,
      peek: peek,
      peekAhead: peekAhead,
      eof: eof,
      croak: croak,
      source: source,
      line: lineNumber,
      column: columnNumber
    };
    function source() { return sourceName }
    function lineNumber() { return line }
    function columnNumber() { return col }
    function next() {
      var ch = input.charAt(pos++);
      if (ch === "\n") {
        line++;
        col = 0;
        //STREAM_META.swap(_.assoc, _.keyword('line'), line);
      }
      else {
        col++;
      }
      //STREAM_META.swap(_.assoc, _.keyword('column'), col);
      return ch;
    }
    function peekAhead() {
      return input.charAt(pos+1);
    }
    function peek() {
      return input.charAt(pos);
    }
    function eof() {
      return peek() == "";
    }
    function croak(msg) {
      throw new Error(msg + " at (" + line + ":" + col + ")");
    }
  }

  var WHITESPACE_CHARS = {
    " ": true,
    "\n": true,
    "\t": true,
    ",": true
  };

  var COLLECTION_START = {
    '(': 'list',
    '[': 'vector',
    '{': 'map',
    '#': 'set'
  };

  var COLLECTION_END = {
    '(': ')',
    '[': ']',
    '{': '}',
    '#': '}',
  };

  var COLLECTION_END_NAME = {
    ')': 'list',
    ']': 'vector',
    '}': 'map',
    //'}': 'set',
  };

  var isUnquoted = _.makeTagPredicate(_.symbol('unquote'));
  var isUnquoteSpliced = _.makeTagPredicate(_.symbol('unquote-splicing'));

  var TYPE_DISPATCH = {
    mori: {
      string: function (rep) { return rep },
      char: function (rep) { return rep },
      symbol: function (rep) {
        if (rep === '/') return _.symbol('/');
        else {
          var parts = rep.split('/');
          if (parts.length === 1) {
            return _.symbol(null, parts[0]);
          }
          else if (parts.length === 2) {
            return _.symbol(parts[0], parts[1]);
          }
          else {
            return _.symbol(parts[0], parts.slice(1).join('/'));
          }
        }
      },
      boolean: function (rep) { return rep === 'true' ? true : false; },
      nil: function (rep) { return null },
      number: function (rep) { return parseFloat(rep) },
      list: function (rep) { return mori.list.apply(mori, rep) },
      vector: function (rep) { return mori.vector.apply(mori, rep) },
      set: function (rep) { return mori.set(rep) },
      map: function (rep) { return mori.hashMap.apply(mori, rep) },
      keyword: function (rep) { return mori.keyword.apply(mori, rep === '/' ? [rep] : rep.split('/')) },
      quote: function (form) { return mori.list(mori.symbol('quote'), form) },
      syntaxquote: function (form) { return mori.list(mori.symbol('syntax-quote'), form) },
      unquote: function (form) { return mori.list(mori.symbol('unquote'), form) },
      unquotesplicing: function (form) { return mori.list(mori.symbol('unquote-splicing'), form) },
      deref: function (form) { return mori.list(mori.symbol('deref'), form) }
    },
    js: {
      string: function (rep) { return rep },
      char: function (rep) { return rep },
      symbol: function (rep) { return Symbol(rep) },
      boolean: function (rep) { return rep === 'true' ? true : false; },
      nil: function (rep) { return null },
      number: function (rep) { return parseFloat(rep) },
      list: function (rep) { return rep },
      vector: function (rep) { return rep },
      set: buildSet,
      map: buildMap,
      keyword: function (rep) { return rep },
      quote: function (form) { return ['quote', form] },
      syntaxquote: function (form) { return ['syntax-quote', form] },
      unquote: function (form) { return ['unquote', form] },
      unquotesplicing: function (form) { return ['unquote-splicing', form] },
      deref: function (form) { return ['deref', form] }
    }
  };


  var ESCAPE_CHARS = {
    n: "\n",
    r: "\r",
    t: "\t",
    v: "\v",
    b: "\b",
    f: "\f"
  };

  function buildSet(rep) {
    var set = {};
    for (var i = 0; i < rep.length; ++i) {
      set[rep[i]] = true;
    }
    return set;
  }

  function buildMap(rep) {
    var pairs = pair(rep);
    var map = {};
    for (var i = 0; i < pairs.length; ++i) {
      map[pairs[i][0]] = pairs[i][1];
    }
    return map;
  }

  // TODO: add rationals, dates (#inst ...) and regexes
  function tokenStream(input, dispatch) {
    var dispatch = dispatch || TYPE_DISPATCH.mori;
    var current = null;
    return {
      next: next,
      peek: peek,
      eof: eof,
      source: input.source,
      line: input.line,
      column: input.column,
      croak: input.croak
    };

    function isDigit(ch) {
      return /[0-9\_\,]/.test(ch);
    }

    function isWhitespace(ch) {
      return !!WHITESPACE_CHARS[ch];
    }

    function isSymbol(ch) {
      return /[a-zA-Z0-9\_\.\/\-\!\?\*\$\=\<\>\&\+\~\|\%]/.test(ch);
    }

    function isChar(ch) {
      return isSymbol(ch);
    }

    function isKeywordStart(ch) {
      return ch === ':';
    }

    function isCollStart(ch) {
      return !!COLLECTION_START[ch];
    }

    function readWhile(pred) {
      var buffer = [];
      while (!input.eof() && pred(input.peek())) {
        buffer.push(input.next());
      }
      return buffer.join('');
    }

    function skipComment() {
      readWhile(function(ch) { return ch != "\n" });
      input.next();
    }

    function readEscaped(end) {
      var escaped = false, buffer = [], xf;
      input.next();
      while (!input.eof()) {
        var ch = input.next();
        if (escaped) {
          if (xf = ESCAPE_CHARS[ch]) {
            buffer.push(xf);
          }
          else {
            buffer.push(ch);
          }
          escaped = false;
        }
        else if (ch == "\\") {
          escaped = true;
        }
        else if (ch == end) {
          break;
        }
        else {
          buffer.push(ch);
        }
      }
      return buffer.join('');
    }

    function readString() {
      return dispatch.string(readEscaped('"'));
    }

    function readNumber(hasSign) {
      var hasDot = false;
      var sign = hasSign ? input.next() : null;
      var num = readWhile(function(ch) {
        if (ch === '.') {
          if (hasDot) throw new Error("unexpexted '.'");
          hasDot = true;
          return true;
        }
        if (hasSign && (ch === '-' || ch === '+')) {
          throw new Error(_.str("unexpected '", ch, "'"));
        }
        return isDigit(ch);
      });
      num = _.str(sign, num.replace(/[,_]/g, ''));
      return dispatch.number(hasDot ? parseFloat(num) : parseInt(num));
    }

    function readSymbol() {
      var sym = readWhile(isSymbol);
      if (sym === 'true' || sym === 'false') {
        return dispatch.boolean(sym);
      }
      else if (sym === 'nil') {
        return dispatch.nil(sym);
      }
      else {
        return dispatch.symbol(sym);
      }
    }

    function readKeyword() {
      input.next();
      var head = '';
      if (input.peek() === ':') {
        head = input.peek();
        input.next();
      }
      var kw = readWhile(isSymbol);
      return dispatch.keyword(_.str(head, kw));
    }

    function readChar() {
      input.next();
      var head = '';
      if (input.peek() === '\\') {
        head = input.peek();
        input.next();
      }
      var ch = readWhile(isChar);
      if (ch.length !== 1) {
        if (ch === 'newline') {
          ch = "\n";
        }
        else if (ch === 'space') {
          ch = " ";
        }
        else if (ch === 'tab') {
          ch = "\t";
        }
        else if (ch === 'return') {
          ch = "\r";
        }
        else if (ch === 'formfeed') {
          ch = "\f";
        }
        else if (ch === 'backspace') {
          ch = "\b";
        }
        else if (ch.startsWith('u')) {
          var u = ['0x', ch.slice(1)].join('');
          ch = String.fromCharCode(1*u);
        }
        else {
          throw new Error(_.str('invalid character: "', ch, '"'));
        }
      }
      return dispatch.char(_.str(head, ch));
    }

    function readCollection(tag, end) {
      var buffer = [];
      if (tag === 'set') input.next();
      while (!input.eof()) {
        var ch = input.next();
        if (ch === end || input.peek() === end) {
          break;
        }
        else {
          buffer.push(readNext());
        }
      }
      return dispatch[tag](buffer);
    }

    function readQuotedForm() {
      input.next();
      return dispatch.quote(readNext());
    }

    function expandList(col, env) {
      return col;
    }

    function readUnquoted() {
      input.next();
      if (input.peek() === '@') {
        input.next();
        return _.list(_.symbol('unquote-splicing'), readNext());
      }
      return _.list(_.symbol('unquote'), readNext());
    }

    function readSyntaxQuote(exp, env) {
      var form = _.second(exp);
      if (_.isList(form)) {
        return _.reverse(
          _.into(
            _.list(),
            _.map(expandList(form), function(x) {
              if (isUnquoteSpliced(x)) {
                throw new Error('splice not in list');
              }
              else if (isUnquoted(x)) {
                return _.second(x);
              }
              else if (_.isList(x)) {
                return readSyntaxQuote(x, env);
              }
              else {
                return x;
              }
        })));
      }
      else {
        return form;
      }
    }

    function readSyntaxQuotedForm() {
      input.next();
      return readSyntaxQuote(readNext());
    }

    function readDerefForm() {
      input.next();
      return dispatch.deref(readNext());
    }

    function readNext() {
      readWhile(isWhitespace);
      if (input.eof()) return null;
      var ch = input.peek();
      var exp;
      if (ch === ";") {
        skipComment();
        exp = readNext();
      }
      else if (ch === '"') {
        exp = readString();
      }
      else if (/[0-9]/.test(ch) || (ch === '-' || ch === '+') && isDigit(input.peekAhead())) {
        exp = readNumber(ch === '-' || ch === '+');
      }
      else if (isSymbol(ch)) {
        exp = readSymbol();
      }
      else if (ch === ':') {
        exp = readKeyword();
      }
      else if (ch === "'") {
        exp = readQuotedForm();
      }
      /*else if (ch === '~') {
        return readUnquoted();
      }
      else if (ch === "`") {
        return readSyntaxQuotedForm();
      }*/
      else if (ch === "@") {
        exp = readDerefForm();
      }
      else if (ch === '\\') {
        exp = readChar();
      }
      else if (isCollStart(ch)) {
        exp = readCollection(COLLECTION_START[ch], COLLECTION_END[ch]);
      }
      else if (!!COLLECTION_END_NAME[ch]) {
        input.next();
        exp = readNext();
      }

      STREAM_META.swap(_.assoc, _.keyword('line'), input.line(), _.keyword('column'), input.column());
      if (_.isUndefined(exp)) {
        input.croak("Can't handle character: '" + ch + "'");
      }
      else {
        STREAM_META.swap(_.assoc, _.keyword('expression'), exp);
        return exp;
      }
    }

    function peek() {
      return current || (current = readNext());
    }

    function next() {
      var token = current;
      current = null;
      return token || readNext();
    }

    function eof() {
      return peek() == null;
    }
  }

  /**
   * @param {string} str
   * @returns {Array<Object>}
   */
  function readString(str, input) {
    if (str == null || typeof str !== 'string') return [];
    var tokens = [];
    return tokenStream(inputStream(str, input || 'string-input'));
  }

  var readFile = null;
  if (typeof exports !== 'undefined') {
    var fs = require('fs');
    /**
     * @param {string} str
     * @returns {Array<Object>}
     */
    var readFileNode = function(file, dispatch) {
      if (file == null || typeof file !== 'string') return [];
      var str = fs.readFileSync(file).toString();
      if (/^#!/.test(str)) {
        str = str.split("\n").slice(1).join("\n");
      }
      return readString(str, file);
    };
  }
  else {
    var readFileBrowser = function(file) {
      var xhr = new XMLHttpRequest();
      var res = null;
      xhr.onreadystatechange = function() {
        if (xhr.readyState === XMLHttpRequest.DONE) {
          if (xhr.status === 200) {
            res = xhr.responseText;
          }
          else {
            throw new Error(_.str("There was an error processing the path: '", file, "'"));
          }
        }
      };
      xhr.open('GET', file, false);
      xhr.send();
      return readString(res, file);
    };
  }

  var readJS = function(exp) {
    if (exp === null || exp === void 0) return null;
    var type = typeof exp;
    var buffer, i, props;
    switch (type) {
      case 'boolean':
      case 'number':
        return exp;
      case 'string':
        if (exp.startsWith('"') && exp.endsWith('"')) {
          return exp.replace(/^"/, '').replace(/"$/, '');
        }
        else if (exp.startsWith(':')) {
          return _.keyword(exp.slice(1));
        } 
        else {
          return _.symbol(exp);
        }
      case 'object':
        if (_.isDate(exp)) return exp;
        else if (_.isRegExp(exp)) return exp;
        else if (_.isArray(exp)) {
          buffer = [];
          for (i = 0; i < exp.length; i++) {
            buffer.push(readJS(exp[i]));
          }
          return _.list.apply(null, buffer);
        }
        else {
          buffer = [];
          props  = Object.getOwnPropertyNames(exp);
          for (i = 0; i < props.length; i++) {
            buffer.push(_.vector(_.keyword(props[i]), readJS(exp[props[i]])));
          }
          return _.into(_.hashMap(), buffer);
        }
      default:
        throw new Error(_.str("'", exp, "' is an invalid expression"));
    }
  };

  var readJSON = function(str) {
    return readJS(JSON.parse(str));
  };

  zera.reader = {
    inputStream: inputStream,
    tokenStream: tokenStream,
    readString: readString,
    readJS: readJS,
    readJSON: readJSON,
    readFile: typeof exports !== 'undefined' ? readFileNode : readFileBrowser,
    TYPE_DISPATCH: TYPE_DISPATCH,
    readSymbol: TYPE_DISPATCH.mori.symbol,
    STREAM_META: STREAM_META
  };

  if (typeof exports !== 'undefined') {
    module.exports = zera.reader;
  }
}
