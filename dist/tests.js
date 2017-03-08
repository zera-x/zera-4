(function() {
  var get = mori.get;
  var readString = pbnj.reader.readString;
  var _ = pbnj.core;

  QUnit.module("pbnj.reader");
  QUnit.test("readString returns an array of objects", function(assert) {
    // TODO: this would be a good candidate for generative testing
    var stream = readString("1 2 3");
    assert.ok(stream.next() === 1, 'returns a token stream this value should be 1');
    assert.ok(stream.next() === 2, 'returns a token stream this value should be 2');
    assert.ok(stream.next() === 3, 'returns a token stream this value should be 3');
  });

  QUnit.test("reading strings", function(assert) {
    var val = "this is a test asfasdfasdfads";
    var v1 = readString(_.str('"', val, '"')).next();
    var col = val.length + 2;
    assert.equal(typeof v1, 'string', "it's a string!");
    assert.equal(v1, val, "it's value is a JS string");
  });

  QUnit.test("reading multiple value & numbers", function(assert) {
    var val = "3.14159 12345";
    var stream = readString(val);
    while (!stream.eof()) {
      assert.equal(typeof stream.next(), 'number', "it's a number!");
    }
  });

  QUnit.test("reading collections", function(assert) {
    var stream = readString('(1 2 3 4) {"a" 5, "b" 6, "c" 7} [8 9 10] #{11, 12, 13} (14 [15, 16] #{17 {18 19}})'); 
    var first = stream.next();
    assert.ok(_.isList(first), "this first is a list...");
    assert.deepEqual(_.intoArray(first), [1, 2, 3, 4], "with values 1, 2, 3, 4.");

    var second = stream.next();
    assert.ok(_.isMap(second), "this second is a hashMap...");
    assert.deepEqual(_.intoArray(_.mapcat(second, _.identity)), ["a", 5, "b", 6, "c", 7], "with values 'a', 5, 'b', 6, 'c', 7.");

    var third = stream.next();
    assert.ok(_.isVector(third), "this second is a vector...");
    assert.deepEqual(_.intoArray(third), [8, 9, 10], "with values 8, 9, 10.");

    var fourth = stream.next();
    assert.ok(_.isSet(fourth), 'set', "this second is a vector...");
    assert.deepEqual(_.intoArray(fourth), [11, 12, 13], "with values 11, 12, 13.");
  });

  QUnit.module('pbnj.wonderscript');
  pbnj.wonderscript.readString("(module 'pbnj.testing)");
  QUnit.test("self evaluating", function(assert) {
    var readString = pbnj.wonderscript.readString;

    var nil = readString('nil');
    assert.ok(nil === null, "nil is null");

    var t = readString('true');
    assert.equal(t, true, "true is true");

    var f = readString('false');
    assert.equal(f, false, "false is false");

    var n0 = readString('1');
    assert.equal(1, n0);

    var n1 = readString('2352353534');
    assert.equal(2352353534, n1);

    var n2 = readString('1.25');
    assert.equal(1.25, n2);

    var sym = readString("'testing");
    assert.ok(_.equals(sym, _.symbol('testing')));

    var kw = readString(":testing");
    assert.ok(_.equals(kw, _.keyword('testing')));

    var values = [1, 2, 3, 4, 5];
    var list = readString(_.str("'(", values.join(' '), ")"));
    var i = 0;
    _.each(list, function(val) {
      assert.equal(val, values[i]);
      i++;
    });

    values = [[_.keyword('a'), 1], [_.keyword('b'), 2], [_.keyword('c'), 3], [_.keyword('d'), 4], [_.keyword('e'), 5]];
    var map = readString(_.str("'{", [].concat.apply([], values).join(' '), "}"));
    i = 0;
    _.each(map, function(pair) {
      var key = _.first(pair);
      var val = _.second(pair);
      assert.ok(_.equals(key, values[i][0]));
      assert.equal(val, values[i][1]);
      i++;
    });

    values = [1, 2, 3, 4, 5];
    i = 0;
    var set = readString(_.str("#{", values.join(' '), "}"));
    var i = 0;
    _.each(set, function(val) {
      assert.equal(val, values[i]);
      i++;
    });
  });

  QUnit.test('variable lookup', function(assert) {
    var readString = pbnj.wonderscript.readString;
  
    assert.throws(function() {
      readString('undefined-variable');
    });

    readString('(define x 1)')
    assert.equal(readString('x'), 1);

    readString('(define y)');
    assert.ok(readString('y') == null, 'null variable');

    readString('(define z nil)');
    assert.ok(readString('z') == null, 'explicit null variable');
  });

  QUnit.test('special forms', function(assert) {
    var readString = pbnj.wonderscript.readString;
    assert.equal(readString('(quote 1)'), 1, "quoted number");
    assert.ok(_.equals(readString('(quote (1 2 3 4))'), _.list(1, 2, 3 ,4)), "quoted list");

    assert.equal(readString('(cond true 1 :else 2)'), 1, "cond first pred true");
    assert.equal(readString('(cond false 1 :else 2)'), 2, "cond else pred");
    assert.equal(readString('(cond (= 1 (add 0 1)) 1 :else 2)'), 1, "cond first compound pred");

  });

  QUnit.test('lambdas', function(assert) {
    var readString = pbnj.wonderscript.readString;
    var ident = readString('(define ident (lambda [x] x))');
    assert.equal(readString('(ident 1)'), 1);
    assert.equal(readString('(ident "adfasdfasd")'), "adfasdfasd");

    var sq = readString('(lambda [x] (mult x x))');
    assert.equal(sq(5), 25);

    readString('(define sq (lambda [x] (mult x x)))');
    assert.equal(readString('(sq 5)'), 25);

    readString('(define sum (lambda [&xs] (apply add xs)))');
    assert.equal(readString('(sum 1 2 3 4)'), (1 + 2 + 3 + 4), "capture variables");
    
    readString('(define rest-args (lambda [x &xs] xs))')
    assert.equal(readString('(apply sum (rest-args 1 2 3 4))'), (2 + 3 + 4), "capture variables");
  });

  QUnit.module('pbnj.jess');
})();
