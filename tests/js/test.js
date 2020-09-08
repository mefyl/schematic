#!/usr/bin/env node

const assert = require('assert');

const { ofString, toString } = require('./test.bc.js');

const value = { i : 42, s : 'enyleme', b: true, a: [0, 1, 2] }

assert.deepEqual(
  ofString('{ "i" : 42, "s": "enyleme", "b": true, "opt": null, "a": [0, 1, 2] }'),
  value)

// Missing optional values are interpreted as None
assert.deepEqual(
  ofString('{ "i" : 42, "s": "enyleme", "b": true, "a": [0, 1, 2] }'),
  value)

delete value['opt']

assert.deepEqual(
  JSON.parse(toString(value)),
  value);
