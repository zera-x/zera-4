(function() {
  var get = mori.get;
  var readString = pbnj.reader.readString;
  var _ = pbnj.core;

  QUnit.module("pbnj.reader");
  QUnit.test("readString returns an array of objects", function(assert) {
    // TODO: this would be a good candidate for generative testing
    var out = readString("1 2 3");
    assert.ok(_.isArray(out), 'returns an array...');
    assert.ok(_.all(out, _.isNumber), 'of numbers');
  });

  QUnit.test("reading strings", function(assert) {
    var val = "this is a test asfasdfasdfads";
    var v1 = readString(_.str('"', val, '"'))[0];
    var col = val.length + 2;
    assert.equal(typeof v1, 'string', "it's a string!");
    assert.equal(v1, val, "it's value is a JS string");
  });

  QUnit.test("reading multiple value & numbers", function(assert) {
    var val = "3.14159 12345";
    var objs = readString(val);
    assert.ok(objs.length === 2, 'returns 2 results');
    _.each(objs, function(obj) {
      assert.equal(typeof obj, 'number', "it's a number!");
    });
  });

  QUnit.test("reading collections", function(assert) {
    var objs = readString('(1 2 3 4) {"a" 5, "b" 6, "c" 7} [8 9 10] #{11, 12, 13} (14 [15, 16] #{17 {18 19}})'); 
    assert.ok(_.isList(objs[0]), "this first is a list...");
    assert.deepEqual(_.intoArray(objs[0]), [1, 2, 3, 4], "with values 1, 2, 3, 4.");

    assert.ok(_.isMap(objs[1]), "this second is a hashMap...");
    assert.deepEqual(_.intoArray(_.mapcat(objs[1], _.identity)), ["a", 5, "b", 6, "c", 7], "with values 'a', 5, 'b', 6, 'c', 7.");

    assert.ok(_.isVector(objs[2]), "this second is a vector...");
    assert.deepEqual(_.intoArray(objs[2]), [8, 9, 10], "with values 8, 9, 10.");

    assert.ok(_.isSet(objs[3]), 'set', "this second is a vector...");
    assert.deepEqual(_.intoArray(objs[3]), [11, 12, 13], "with values 11, 12, 13.");
  });
})();
