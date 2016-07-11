goog.provide('pbnj.reader');

goog.require('pbnj.util');

goog.scope(function() {

  var pair = pbnj.util.pair;

  function inputStream(input) {
    var pos = 0, line = 1, col = 0;
    return {
      next: next,
      peek: peek,
      eof: eof,
      croak: croak
    };
    function next() {
      var ch = input.charAt(pos++);
      if (ch === "\n") {
        line++;
        col = 0;
      }
      else {
        col++;
      }
      return ch;
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
    '{': 'hashMap',
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
    '}': 'hashMap',
    '}': 'set',
  };

  var TYPE_DISPATCH = {
    mori: {
      string: function (rep) { return rep },
      symbol: function (rep) { return mori.symbol(rep) },
      number: function (rep) { return parseFloat(rep) },
      list: function (rep) { return mori.list.apply(mori, rep) },
      vector: function (rep) { return mori.vector.apply(mori, rep) },
      set: function (rep) { return mori.set(rep) },
      hashMap: function (rep) { return mori.hashMap.apply(mori, rep) },
      keyword: function (rep) { return mori.keyword(rep) },
      quote: function (form) { return mori.list(mori.symbol('quote'), form) }
    },
    js: {
      string: function (rep) { return rep },
      symbol: function (rep) { return Symbol(rep) },
      number: function (rep) { return parseFloat(rep) },
      list: function (rep) { return rep },
      vector: function (rep) { return rep },
      set: buildSet,
      hashMap: buildMap,
      keyword: function (rep) { return rep },
      quote: function (form) { return ['quote', form] }
    }
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

  function tokenStream(input, dispatch) {
    var current = null;
    var dispatch = dispatch || TYPE_DISPATCH.mori;
    return {
      next: next,
      peek: peek,
      eof: eof,
      croak: input.croak
    };

    function isDigit(ch) {
      return /[0-9]/.test(ch);
    }

    function isWhitespace(ch) {
      return !!WHITESPACE_CHARS[ch];
    }

    function isSymbol(ch) {
      return /[a-zA-Z0-9_\.\/\-\!\?\*\$\=\<\>\&\+]/.test(ch);
    }

    function isKeywordStart(ch) {
      return ch === ':';
    }

    function isQuoteStart(ch) {
      return ch === "'";
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
      var escaped = false, buffer = [];
      input.next();
      while (!input.eof()) {
        var ch = input.next();
        if (escaped) {
          buffer.push(ch);
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

    function readNumber() {
      var hasDot = false;
      var num = readWhile(function(ch) {
        if (ch === '.') {
          if (hasDot) return false;
          hasDot = true;
          return true;
        }
        return isDigit(ch);
      });
      return dispatch.number((num));
    }

    function readSymbol() {
      var sym = readWhile(isSymbol);
      return dispatch.symbol(sym);
    }

    function readKeyword() {
      input.next();
      var kw = readWhile(isSymbol);
      return dispatch.keyword(kw);
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

    function readNext() {
      readWhile(isWhitespace);
      if (input.eof()) return null;

      var ch = input.peek();
      if (ch == ";") {
        skipComment();
        return readNext();
      }
      else if (ch == '"') {
        return readString();
      }
      else if (isDigit(ch)) {
        return readNumber();
      }
      else if (isSymbol(ch)) {
        return readSymbol();
      }
      else if (ch === ':') {
        return readKeyword();
      }
      else if (ch === "'") {
        return readQuotedForm();
      }
      else if (isCollStart(ch)) {
        return readCollection(COLLECTION_START[ch], COLLECTION_END[ch]);
      }
      else if (!!COLLECTION_END_NAME[ch]) {
        input.next();
        return readNext();
      }
      else {
        input.croak("Can't handle character: '" + ch + "'");
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

  function readString(str, dispatch) {
    var tokens = [];
    var stream = tokenStream(inputStream(str), dispatch);
    while (!stream.eof()) tokens.push(stream.next());
    return tokens;
  }

  pbnj.reader = {
    inputStream: inputStream,
    tokenStream: tokenStream,
    readString: readString,
    TYPE_DISPATCH: TYPE_DISPATCH
  }

});
