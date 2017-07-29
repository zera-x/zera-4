"use strict";
exports.__esModule = true;
var _ = require("mori");
var pbnj;
(function (pbnj) {
    var core;
    (function (core) {
        core.DEBUG = false;
        // merge mori's API
        function extend(target, source, pred) {
            for (var key in source) {
                if (!pred || pred(source[key], key, source, target))
                    target[key] = source[key];
            }
        }
        core.extend = extend;
        _['->string'] = function (obj) { return '' + obj; };
        function makeTagPredicate(tag) {
            return function (exp) {
                return _.isList(exp) && _.isSymbol(_.first(exp)) && _.equals(_.first(exp), tag);
            };
        }
        core.makeTagPredicate = makeTagPredicate;
        var isQuoted = makeTagPredicate(_.symbol('quote'));
        function inspect(exp) {
            var i, buffer, props;
            if (exp == null) {
                return 'nil';
            }
            else if (_.isBoolean(exp)) {
                return exp === true ? 'true' : 'false';
            }
            else if (_.isString(exp)) {
                return ['"', exp, '"'].join('');
            }
            else if (isQuoted(exp)) {
                return ["'", _.inspect(_.second(exp))].join('');
            }
            else if (_.isArray(exp)) {
                buffer = [];
                for (i = 0; i < exp.length; i++) {
                    buffer.push(_.inspect(exp[i]));
                }
                if (buffer.length === 0) {
                    return "(array)";
                }
                else {
                    return ["(array ", buffer.join(' '), ")"].join('');
                }
            }
            else if (_.isObject(exp)) {
                if (_.isFunction(exp.toString)) {
                    return exp.toString();
                }
                else {
                    props = Object.getOwnPropertyNames(exp);
                    buffer = [];
                    for (i = 0; i < props.length; i++) {
                        buffer.push([props[i], _.inspect(exp[props[i]])]);
                    }
                    if (buffer.length === 0) {
                        return "(js/object)";
                    }
                    else {
                        var vars = [];
                        for (i = 0; i < buffer.length; i++) {
                            vars.push([":", buffer[i][0], ' ', buffer[i][1]].join(''));
                        }
                        return ["(js/object ", vars.join(', '), ")"].join('');
                    }
                }
            }
            else {
                return '' + exp;
            }
        }
        core.inspect = inspect;
        ;
        function makeType(name, meta, ctr) {
            var fmtVar, joinComma, varStr, meta, types;
            meta = _.merge(_.hashMap(_.symbol('type'), true), meta);
            fmtVar = function (x) { return [x[0], ': ', _.inspect(x[1])].join(''); };
            joinComma = function (s, x) { return [s, ', ', x].join(''); };
            if (types = _.get(meta, _.keyword('types'))) {
                types = _.union(types, _.set([ctr]));
            }
            else {
                types = _.set([ctr]);
            }
            ctr.toString = function () {
                return name.toString();
            };
            ctr.getMeta = function () {
                return meta;
            };
            ctr.prototype.types = function () {
                return types;
            };
            ctr.prototype['class'] = function () {
                return ctr;
            };
            ctr.prototype['isa'] = function (klass) {
                return _.has(types, klass);
            };
            ctr.prototype.toString = function () {
                var props = [];
                for (var prop in this) {
                    if (this.hasOwnProperty(prop)) {
                        props.push([prop, this[prop]]);
                    }
                }
                varStr = _.reduce(joinComma, _.map(fmtVar, props));
                return _.str('#<', name, ' ', varStr, '>');
            };
            return ctr;
        }
        core.makeType = makeType;
        core.Atom = _.makeType(_.symbol('pbnj.core', 'Atom'), _.hashMap(), function (value, meta, validator) {
            this.meta = meta;
            this.value = value;
            this.watches = {};
            if (_.isFunction(validator)) {
                this.validator = validator;
            }
        });
        core.Atom.prototype.deref = function () {
            return this.value;
        };
        core.Atom.prototype.reset = function (val) {
            var keys = Object.getOwnPropertyNames(this.watches);
            var i;
            for (i = 0; i < keys.length; i++) {
                this.watches[keys[i]].call(null, keys[i], this, this.value, val);
            }
            if (!this.validator || this.validator.call(null, val) === true) {
                this.value = val;
            }
            else {
                throw new Error('not a valid value');
            }
            return this;
        };
        core.Atom.prototype.swap = function (fn) {
            return this.reset(fn.apply(null, [this.value].concat(Array.prototype.slice.call(arguments, 1))));
        };
        core.Atom.prototype.addWatch = function (key, f) {
            if (!_.isFunction(f))
                throw new Error('second argument should be a function');
            this.watches[key] = f;
            return this;
        };
        /**
         * @constructor
         * @final
         */
        core.Var = _.makeType(_.symbol('pbnj.core', 'Var'), _.hashMap(), function (value, meta) {
            this.value = value;
            this.meta = meta || _.hashMap();
            this.notset = value == null ? true : false;
        });
        core.Var.prototype.getMeta = function () {
            return this.meta;
        };
        core.Var.prototype.set = function (value) {
            if (this.notset || this.isDynamic()) {
                this.value = value;
                this.notset = false;
            }
            else {
                console.warn("A Var can only be set once unless it has been tagged :dynamic");
            }
            return this;
        };
        core.Var.prototype.get = function () {
            return this.value;
        };
        core.Var.prototype.deref = core.Var.prototype.getValue;
        core.Var.prototype.withMeta = function (meta) {
            this.meta = meta;
            return this;
        };
        core.Var.prototype.varyMeta = function (f, args) {
            this.meta = f.apply(null, [this.meta].concat(_.intoArray(args)));
            return this;
        };
        core.Var.prototype.isMacro = function () {
            return this.meta && _.get(this.meta, _.keyword('macro'));
        };
        core.Var.prototype.isAlias = function () {
            return this.meta && _.get(this.meta, _.keyword('ns', 'alias'));
        };
        core.Var.prototype.isNamespace = function () {
            return this.meta && _.equals(_.get(this.meta, _.keyword('tag')), _.symbol('pbnj.wonderscript', 'Namespace'));
        };
        core.Var.prototype.isClass = function () {
            return this.meta && _.get(this.meta, _.keyword('type'));
        };
        core.Var.prototype.isDynamic = function () {
            return this.meta && _.get(this.meta, _.keyword('dynamic'));
        };
        /**
         * @param {*} val
         * @returns {boolean}
         */
        _.isNumber = _['number?'] = function (val) {
            return Object.prototype.toString.call(val) === '[object Number]';
        };
        /**
         * @param {*} val
         * @returns {boolean}
         */
        _.isString = _['string?'] = function (val) {
            return Object.prototype.toString.call(val) === '[object String]';
        };
        /**
         * @param {*} val
         * @returns {boolean}
         */
        _.isBoolean = _['boolean?'] = function (val) {
            return val === true || val === false || Object.prototype.toString.call(val) === '[object Boolean]';
        };
        /**
         * @param {*} val
         * @returns {boolean}
         */
        _.isNull = _['nil?'] = function (val) {
            return val === null;
        };
        /**
         * @param {*} val
         * @returns {boolean}
         */
        _.isUndefined = _['undefined?'] = function (val) {
            return val === void 0;
        };
        /**
         * @param {*} val
         * @returns {boolean}
         */
        _.isDate = _['date?'] = function (val) {
            return Object.prototype.toString.call(val) === '[object Date]';
        };
        /**
         * @param {*} val
         * @returns {boolean}
         */
        _.isError = _['error?'] = function (val) {
            return Object.prototype.toString.call(val) === '[object Error]';
        };
        /**
         * @param {*} val
         * @returns {boolean}
         */
        _.isRegExp = _['regexp?'] = function (val) {
            return Object.prototype.toString.call(val) === '[object RegExp]';
        };
        /**
         * @param {*} val
         * @returns {boolean}
         */
        _.isFunction = _['function?'] = function (val) {
            return Object.prototype.toString.call(val) === '[object Function]';
        };
        /**
         * @param {*} val
         * @returns {boolean}
         */
        _.isArguments = _['arguments?'] = function (val) {
            return Object.prototype.toString.call(val) === '[object Arguments]';
        };
        /**
         * @param {*} val
         * @returns {boolean}
         */
        _.isElement = _['element?'] = function (obj) {
            return !!(obj && obj.nodeType === 1);
        };
        _['='] = _.equals;
        _['identical?'] = function (a, b) { return a === b; };
        _['equiv?'] = function (a, b) { return a == b; };
        _['not'] = function (x) { return !x; };
        _['>'] = function (a, b) { return a > b; };
        _['<'] = function (a, b) { return a < b; };
        _['<='] = function (a, b) { return a <= b; };
        _['>='] = function (a, b) { return a >= b; };
        _.mod = function (a, b) { return a % b; };
        _.array = function () { return Array.prototype.slice.call(arguments); };
        _.object = function () { return {}; };
        _.println = console.log.bind(console);
        _.name = function (sym) { return sym.name; };
        _.namespace = function (sym) { return sym.ba; };
        _.get = function (col, key, alt) {
            if (_.isCollection(col)) {
                return _.get(col, key, alt);
            }
            else {
                var val = col[key];
                return val != null
                    ? val
                    : alt == null
                        ? null
                        : alt;
            }
        };
        _['class-name'] = _.getClassName = function (obj) {
            function get_class(obj) {
                return "".concat(obj).replace(/^.*function\s+([^\s]*|[^\(]*)\([^\x00]+$/, "$1") || "anonymous";
            }
            ;
            var result = "";
            if (obj == null) {
                result = "nil";
            }
            else {
                result = get_class(obj.constructor);
                if (result === "Object" && obj.constructor.prototype) {
                    for (result in this) {
                        if (typeof (this[result]) === "function" && obj instanceof this[result]) {
                            result = get_class(this[result]);
                            break;
                        }
                    }
                }
            }
            return result;
        };
        _.isa = function isa(obj, className) {
            if (_.isSymbol(className)) {
                var className = className.toString();
            }
            if (_.isString(className)) {
                return _.getClassName(obj) === className;
            }
            else if (_.isFunction(className)) {
                return obj instanceof eval(className);
            }
            else {
                throw new Error("class must be a string, symbol, or constructor");
            }
        };
        /** @interface */
        pbnj.IArray = function () { };
        pbnj.IArray.prototype.length = 0;
        /** @typedef {(Array<*>|pbnj.IArray)} */
        pbnj.ArrayLike;
        var optimizeCb = function (func, context, argCount) {
            if (context === void 0)
                return func;
            switch (argCount == null ? 3 : argCount) {
                case 1: return function (value) {
                    return func.call(context, value);
                };
                case 2: return function (value, other) {
                    return func.call(context, value, other);
                };
                case 3: return function (value, index, collection) {
                    return func.call(context, value, index, collection);
                };
                case 4: return function (accumulator, value, index, collection) {
                    return func.call(context, accumulator, value, index, collection);
                };
            }
            return function () {
                return func.apply(context, arguments);
            };
        };
        /**
         * Convert an Array like object into an instance of Array.
         *
         * @param {(pbnj.ArrayLike|null)} obj
         * @returns {Array<*>}
         */
        _.toArray = _['->array'] = function (obj) {
            if (!obj)
                return [];
            else if (_.isArray(obj))
                return obj;
            else if (_.isCollection(obj)) {
                return _.intoArray(obj);
            }
            else {
                return Array.prototype.slice.call(obj);
            }
        };
        /**
         * @params {Object} obj
         * @returns {boolean}
         */
        _.isArray = _['array?'] = (Array.isArray || function (obj) {
            return Object.prototype.toString.call(obj) === '[object Array]';
        });
        var MAX_ARRAY_INDEX = Math.pow(2, 53) - 1;
        /**
         * @param {Object} obj
         * @returns {boolean}
         */
        _.isArrayLike = _['arraylike?'] = function (obj) {
            var length = obj && obj.length;
            return typeof length === 'number' && length >= 0 && length <= MAX_ARRAY_INDEX;
        };
        // TODO: it'd probobly be better to have these mappings in core.ws
        //  _['into-array'] = _.intoArray;
        //  _['map-indexed'] = _.mapIndexed;
        //
        //  _['symbol?'] = _.isSymbol;
        //  _['keyword?'] = _.isKeyword;
        //  _['list?'] = _.isList;
        //  _['map?'] = _.isMap;
        //  _['vector?'] = _.isVector;
        //  _['set?'] = _.isSet;
        //  _['collection?'] = _.isCollection;
        //  _['seq?'] = _.isSeq;
        //  _['sequential?'] = _.isSequential;
        //  _['associative?'] = _.isAssociative;
        //  _['counted?'] = _.isCounted;
        //  _['indexed?'] = _.isIndexed;
        //  _['reduceable?'] = _.isReduceable;
        //  _['seqable?'] = _.isSeqable;
        //  _['reversable?'] = _.isReversable;
        //
        //  _['empty?'] = _.isEmpty;
        //  _['odd?'] = _.isOdd;
        //  _['even?'] = _.isEven;
        //  _['subset?'] = _.isSubset;
        //  _['superset?'] = _.isSuperset;
        //
        //  _['has?'] = _.hasKey;
        //  _['has-key?'] = _.hasKey;
        //
        //  _['hash-map'] = _.hashMap;
        //  _['sorted-set'] = _.sortedSet;
        //
        //  _['get-in'] = _.getIn;
        //  _['assoc-in'] = _.assocIn;
        //  _['update-in'] = _.updateIn;
        //
        //  _['reduce-kv'] = _.reduceKV;
        //  _['take-while'] = _.takeWhile;
        //  _['drop-while'] = _.dropWhile;
        //  _['sort-by'] = _.sortBy;
        //  _['partition-by'] = _.partitionBy;
        //  _['group-by'] = _.groupBy;
        //
        //  _['prim-seq'] = _.primSeq;
        //  _['->ws'] = _.toClj;
        //  _['->js'] = _.toJs;
        /**
         * @param {pbnj.ArrayLike} obj
         * @returns {number}
         */
        _.size = function (obj) {
            if (obj == null)
                return 0;
            return _.isArrayLike(obj) ? obj.length : _.keys(obj).length;
        };
        /**
         * @param {*} val
         * @returns {*}
         */
        _.identity = function (val) { return val; };
        /**
         * @param {*} val
         * @returns {Function}
         */
        _.always = function (val) {
            return function () { return val; };
        };
        /**
         * @param {Object} obj
         * @param {Function} fn
         * @returns {{}}
         */
        _.mapObject = _['map-object'] = function (obj, fn) {
            if (obj == null)
                return {};
            var keys = _.keys(obj);
            var results = {};
            for (var i = 0; i < keys.length; ++i) {
                results[keys[i]] = fn(obj[keys[i]], keys[i], obj);
            }
            return results;
        };
        _['object-keys'] = function (obj) {
            if (obj == null) {
                return _.list();
            }
            else {
                return _.list.apply(null, Object.keys(obj));
            }
        };
        _['object-values'] = function (obj) {
            if (obj == null) {
                return _.list();
            }
            else {
                return _.list.apply(null, Object.values(obj));
            }
        };
        _.objectToMap = _['object->map'] = function (obj) {
            if (obj == null) {
                return _.hashMap();
            }
            var keys = Object.keys(obj);
            var buffer = [];
            for (var i = 0; i < keys.length; i++) {
                buffer.push(_.keyword(keys[i]));
                buffer.push(obj[keys[i]]);
            }
            return _.hashMap.apply(null, buffer);
        };
        _['map->object'] = function (map) {
            if (map === null)
                return {};
            var obj = {};
            mori.each(map, function (x) {
                var k = mori.nth(x, 0);
                var v = mori.nth(x, 1);
                if (mori.isKeyword(k)) {
                    k = k.toString().replace(/^:/, '');
                }
                obj[k] = v;
            });
            return obj;
        };
        _.objectToVector = _['object->vector'] = function (obj) {
            if (obj == null) {
                return _.vector();
            }
            var keys = Object.keys(obj);
            var buffer = [];
            for (var i = 0; i < keys.length; i++) {
                buffer.push(_.vector(_.keyword(keys[i]), obj[keys[i]]));
            }
            return _.vector.apply(null, buffer);
        };
        _.objectToList = _['object->list'] = function (obj, filter) {
            if (obj == null) {
                return _.list();
            }
            var keys = Object.keys(obj);
            var buffer = [];
            for (var i = 0; i < keys.length; i++) {
                var k = _.keyword(keys[i]);
                if (filter && _.isFalse(filter.call(null, k))) {
                    continue;
                }
                var pair = _.vector(k, obj[keys[i]]);
                buffer.push(pair);
            }
            return _.list.apply(null, buffer);
        };
        _.arrayToList = _['array->list'] = function (a) {
            if (a == null) {
                return _.list();
            }
            return _.list.apply(null, a);
        };
        _.arrayToVector = _['array->vector'] = function (a) {
            if (a == null) {
                return _.vector();
            }
            return _.vector.apply(null, a);
        };
        /**
         * @param {Function} func
         * @param {?Function} hasher
         * @returns {Function}
         */
        _.memoize = function (func, hasher) {
            var memoize = function (key) {
                var cache = memoize.cache;
                var address = '' + (hasher ? hasher.apply(this, arguments) : key);
                if (!_.has(cache, address))
                    cache[address] = func.apply(this, arguments);
                return cache[address];
            };
            memoize.cache = {};
            return memoize;
        };
        /**
         * @param {Object} obj
         * @returns {boolean}
         */
        _.isObject = _['object?'] = function (obj) {
            var type = typeof obj;
            return type === 'function' || type === 'object' && !!obj;
        };
        /**
         * @param {Object} obj
         * @param {string} key
         * @returns {*}
         */
        _.has = _.hasKey = function (obj, key) {
            if (mori.isCollection(obj)) {
                return mori.hasKey(obj, key);
            }
            else {
                return obj != null && Object.prototype.hasOwnProperty.call(obj, key);
            }
        };
        /**
         * @params {Object}
         * @returns {Array<*>}
         */
        _.keys = function (obj) {
            if (mori.isAssociative(obj)) {
                return mori.keys(obj);
            }
            else {
                if (!_.isObject(obj))
                    return [];
                if (Object.keys)
                    return Object.keys(obj);
                var keys = [];
                for (var key in obj)
                    if (_.has(obj, key))
                        keys.push(key);
                return keys;
            }
        };
        /**
         * @params {Object}
         * @returns {Array<*>}
         */
        _.values = function (obj) {
            if (mori.isAssociative(obj)) {
                return mori.values(obj);
            }
            else {
                if (!_.isObject(obj))
                    return [];
                if (Object.values)
                    return Object.values(obj);
                var values = [];
                for (var key in obj)
                    if (_.has(obj, key))
                        values.push(obj[key]);
                return values;
            }
        };
        /**
         * Coerce values into strings and concatenate arguments
         *
         * @param {...*} args
         * @returns {string}
         */
        _.str = function () {
            if (arguments.length === 0)
                return '';
            else if (arguments.length === 1)
                return '' + arguments[0];
            else {
                return Array.prototype.slice.call(arguments).join('');
            }
        };
        /**
         * @param {*} val
         * @returns {boolean}
         */
        _.exists = function (val) { return val != null; };
        _.isFalse = _['false?'] = function (val) {
            return val === false || _.isNull(val) || _.isUndefined(val);
        };
        /**
         * @params {Array<*>}
         * @returns {Array<Array<*>>}
         */
        _.pair = function (a) {
            var i, pairs;
            if (mori.isCollection(a)) {
                pairs = mori.vector();
                for (i = 0; i < mori.count(a); i += 2) {
                    pairs = mori.conj(pairs, mori.vector(mori.nth(a, i), mori.nth(a, i + 1)));
                }
            }
            else {
                pairs = [];
                for (i = 0; i < a.length; i += 2) {
                    pairs.push([a[i], a[i + 1]]);
                }
            }
            return pairs;
        };
        var flatten = function (input, shallow, strict, startIndex) {
            var output = [], idx = 0;
            for (var i = startIndex || 0; i < input.length; i++) {
                var value = input[i];
                if (_.isArrayLike(value) && (_.isArray(value) || _.isArguments(value))) {
                    if (!shallow)
                        value = flatten(value, shallow, strict);
                    var j = 0, len = value.length;
                    output.length += len;
                    while (j < len) {
                        output[idx++] = value[j++];
                    }
                }
                else if (!strict) {
                    output[idx++] = value;
                }
            }
            return output;
        };
        _.flatten = function (array, shallow) {
            return flatten(array, shallow, false);
        };
        /**
         *
         * @param {pbnj.ArrayLike} obj
         * @param {Function} fn
         * @param {?Object} context
         * @returns {*}
         */
        _.each = _.forEach = function (obj, fn) {
            if (mori.isCollection(obj)) {
                mori.each(obj, fn);
                return obj;
            }
            else {
                var iteratee = optimizeCb(fn);
                var i, length;
                if (_.isArrayLike(obj)) {
                    for (i = 0, length = obj.length; i < length; i++) {
                        iteratee(obj[i], i, obj);
                    }
                }
                else {
                    var keys = _.keys(obj);
                    for (i = 0, length = keys.length; i < length; i++) {
                        iteratee(obj[keys[i]], keys[i], obj);
                    }
                }
                return obj;
            }
        };
        /**
         * @param {pbnj.ArrayLike} a
         * @param {string} prop
         * @param {?Object} context
         * @returns {Array<*>}
         */
        _.pluck = function (a, prop) {
            return _.map(function (val) { return val[prop]; }, a);
        };
        /**
         * @param {pbnj.ArrayLike} a
         * @param {string} prop
         * @param {...*}
         * returns {Array<*>}
         */
        _.invoke = function (a, prop) {
            var args = _.toArray(arguments).slice(2);
            return _.map(function (val) { return val[prop].apply(val, args); }, a);
        };
        /**
         * @param {pbnj.ArrayLike} a
         * @param {Function} fn
         * @returns {Array<*>}
         */
        /*_.mapcat = function (a, fn) {
          if (mori.isCollection(a)) {
            return mori.mapcat(fn, a);
          }
          else {
            var a = _.map(fn, a), newA = [], i;
            for (i = 0; i < a.length; ++i) {
              newA = newA.concat(a[i]);
            }
            return newA;
          }
        };*/
        /**
         * @param {pbnj.ArrayLike} obj
         * @param {Function} fn
         * @param {?Object} context
         * @returns {Array<*>}
         */
        /*_.filter = function (obj, fn) {
          if (mori.isCollection(obj)) {
            return mori.filter(fn, obj);
          }
          else {
            var iteratee = optimizeCb(fn);
            var i, results = [];
            if (_.isArrayLike(obj)) {
              for (i = 0; i < obj.length; ++i) {
                if (iteratee(obj[i], i)) results.push(obj[i]);
              }
            }
            else {
              var keys = _.keys(obj);
              for (i = 0; i < keys.length; ++i) {
                if (iteratee(obj[keys[i]], keys[i])) results.push(obj[keys[i]]);
              }
            }
            return results;
          }
        };*/
        /**
         * @param {pbnj.ArrayLike} obj
         * @param {Function} fn
         * @param {?Object} context
         * @returns {Array<*>}
         */
        /*_.reject = _.remove = function (obj, fn) {
          if (mori.isCollection(obj)) {
            return mori.remove(fn, obj);
          }
          else {
            var iteratee = optimizeCb(fn);
            var i, results = [];
            if (_.isArrayLike(obj)) {
              for (i = 0; i < obj.length; ++i) {
                if (!iteratee(obj[i], i)) results.push(obj[i]);
              }
            }
            else {
              var keys = _.keys(obj);
              for (i = 0; i < keys.length; ++i) {
                if (!iteratee(obj[keys[i]], keys[i])) results.push(obj[keys[i]]);
              }
            }
            return results;
          }
        };*/
        /**
         * @param {pbnj.ArrayLike} obj
         * @param {Function} fn
         * @param {?*} memo
         * @param {?Object} context
         * @returns {*}
         */
        /*_.reduce = _.foldl = function (obj, fn, memo) {
          if (mori.isCollection(obj)) {
            return memo === void 0 ? mori.reduce(fn, obj) : mori.reduce(fn, memo, obj);
          }
          else {
            var iteratee = optimizeCb(fn, null, 4);
            var i;
            if (_.isArrayLike(obj)) {
              for (i = 0; i < obj.length; ++i) {
                if (memo == null && i === 0) memo = obj[i];
                memo = iteratee(memo, obj[i], i, obj);
              }
            }
            else {
              var keys = _.keys(obj);
              for (i = 0; i < keys.length; ++i) {
                if (memo == null && i === 0) memo = obj[keys[i]];
                memo = iteratee(memo, obj[keys[i]], keys[i], obj);
              }
            }
            return memo;
          }
        };*/
        /**
         * @param {pbnj.ArrayLike} obj
         * @param {Function} fn
         * @param {?*} memo
         * @param {?Object} context
         * @returns {*}
         */
        _.reduceRight = _.foldr = function (obj, fn, memo, context) {
            if (mori.isCollection(obj)) {
                return mori.reduceRight(fn, obj);
            }
            else {
                var iteratee = optimizeCb(fn, context, 4);
                var i, init;
                if (_.isArrayLike(obj)) {
                    init = (obj.length - 1);
                    for (i = init; i >= 0; --i) {
                        if (memo == null && i === init)
                            memo = obj[i];
                        memo = iteratee(memo, obj[i], i, obj);
                    }
                }
                else {
                    var keys = _.keys(obj);
                    init = (keys.length - 1);
                    for (i = init; i >= 0; --i) {
                        if (memo == null && i === init)
                            memo = obj[keys[i]];
                        memo = iteratee(memo, obj[keys[i]], keys[i], obj);
                    }
                }
                return memo;
            }
        };
        _.min = function (a) {
            return _.reduce(a, function (memo, n) {
                return memo < n ? memo : n;
            }, null);
        };
        _.max = function (a) {
            return _.reduce(a, function (memo, n) {
                return memo > n ? memo : n;
            }, null);
        };
        _.any = function (a, fn) {
            return _.filter(a, fn).length !== 0;
        };
        _.all = function (a, fn) {
            return a.length === _.filter(a, fn).length;
        };
        // Array -> Array
        _.uniq = function (a) {
            var set = _.into(_.set(), a);
            return _.intoArray(set);
        };
    })(core = pbnj.core || (pbnj.core = {}));
})(pbnj || (pbnj = {})); // namespace
