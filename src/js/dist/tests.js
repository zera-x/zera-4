(function() {
  var get = mori.get;
  var readString = pbnj.reader.readString;
  var _ = pbnj.core;

  QUnit.module("pbnj.reader");
  QUnit.test("readString returns an array of syntax objects", function(assert) {
    // TODO: this would be a good candidate for generative testing
    var out = readString("1 2 3");
    assert.ok(_.isArray(out), 'returns an array...');
    assert.ok(_.all(out, _.isSyntax), 'of syntax objects');
  });

  QUnit.test("reading strings", function(assert) {
    var val = "this is a test asfasdfasdfads";
    var v1 = readString(_.str('"', val, '"'))[0];
    var col = val.length + 2;
    assert.equal(get(v1, 'type'), 'string', "it's a string!");
    assert.equal(get(v1, 'value'), val, "it's value is a JS string");
    assert.equal(get(v1, 'source'), 'input', "it's source is 'input' since it wasn't read from a file");
    assert.equal(get(v1, 'line'), 1, "it's line is 1");
    assert.equal(get(v1, 'column'), col, _.str("it's column is ", col));
  });

  QUnit.test("reading multiple value & numbers", function(assert) {
    var val = "3.14159 12345";
    var syns = readString(val);
    assert.ok(syns.length === 2, 'returns 2 results');
    _.each(syns, function(syn) {
      assert.equal(get(syn, 'type'), 'number', "it's a number!");
      assert.ok(typeof get(syn, 'value') === 'number', "it's value is a JS number");
    });

    QUnit.test("reading collections", function(assert) {
      var syns = readString('(1 2 3 4) {"a" 5, "b" 6, "c" 7} [8 9 10] #{11, 12, 13} (14 [15, 16] #{17 {18 19}})'); 
      assert.equal(get(syns[0], 'type'), 'list', "this first is a list...");
      assert.ok(_.all(get(syns[0], 'value'), _.isSyntax), "the value is an Array of syntax objects...");
      assert.deepEqual(_.pluck(get(syns[0], 'value'), 'value'), [1, 2, 3, 4], "with values 1, 2, 3, 4.");

      assert.equal(get(syns[1], 'type'), 'hashMap', "this second is a hashMap...");
      assert.ok(_.all(get(syns[1], 'value'), _.isSyntax), "the value is an Array of syntax objects...");
      assert.deepEqual(_.pluck(get(syns[1], 'value'), 'value'), ["a", 5, "b", 6, "c", 7], "with values 'a', 5, 'b', 6, 'c', 7.");

      assert.equal(get(syns[2], 'type'), 'vector', "this second is a vector...");
      assert.ok(_.all(get(syns[2], 'value'), _.isSyntax), "the value is an Array of syntax objects...");
      assert.deepEqual(_.pluck(get(syns[2], 'value'), 'value'), [8, 9, 10], "with values 8, 9, 10.");

      assert.equal(get(syns[3], 'type'), 'set', "this second is a vector...");
      assert.ok(_.all(get(syns[3], 'value'), _.isSyntax), "the value is an Array of syntax objects...");
      assert.deepEqual(_.pluck(get(syns[3], 'value'), 'value'), [11, 12, 13], "with values 11, 12, 13.");
    });
  });
}());
