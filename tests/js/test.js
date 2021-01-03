#!/usr/bin/env node

const assert = require('assert');

const { toJs, ofJs, ofString, toString, variantRoundtrip } = require('./test.bc.js');

const value = { i : 42, s : 'enyleme', b: true, a: [0, 1, 2], pair: [true, false] }

// Optional missing
assert.deepEqual(
  ofJs({ "i" : 42, "s": "enyleme", "b": true, "a": [0, 1, 2], "pair": [true, false] }),
  value)
assert.deepEqual(
  ofString('{ "i" : 42, "s": "enyleme", "b": true, "a": [0, 1, 2], "pair": [true, false] }'),
  value)

// Optional as null
assert.deepEqual(
  ofString('{ "i" : 42, "s": "enyleme", "b": true, "opt": null, "opt_nullable": null, "a": [0, 1, 2], "pair": [true, false] }'),
  Object.assign({}, value, { opt_nullable: null }))
assert.deepEqual(
  ofJs({ "i" : 42, "s": "enyleme", "b": true, "opt": null, "opt_nullable": null, "a": [0, 1, 2], "pair": [true, false] }),
  Object.assign({}, value, { opt_nullable: null }))

// Optional filled
assert.deepEqual(
  ofString('{ "i" : 42, "s": "enyleme", "b": true, "opt": true, "opt_nullable": false, "a": [0, 1, 2], "pair": [true, false] }'),
  Object.assign({}, value, { opt: true, opt_nullable: false }))
assert.deepEqual(
  ofJs({ "i" : 42, "s": "enyleme", "b": true, "opt": true, "opt_nullable": false, "a": [0, 1, 2], "pair": [true, false] }),
  Object.assign({}, value, { opt: true, opt_nullable: false }))

assert.deepEqual(
  toJs(value),
  value);
assert.deepEqual(
  JSON.parse(toString(value)),
  value);

assert.deepEqual(variantRoundtrip(24), 24)
assert.deepEqual(variantRoundtrip("this"), "this")
assert.throws(() => variantRoundtrip("that"))
