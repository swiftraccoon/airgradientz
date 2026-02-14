'use strict';

// Test fixtures based on real AirGradient device responses and
// edge cases from the python-airgradient reference library.

const indoorFull = Object.freeze({
  wifi: -51,
  serialno: '84fce602549c',
  rco2: 489,
  pm01: 23.83,
  pm02: 41.67,
  pm10: 54.5,
  pm003Count: 5006.5,
  pm02Compensated: 31.18,
  atmp: 20.78,
  atmpCompensated: 20.78,
  rhum: 32.19,
  rhumCompensated: 32.19,
  tvocIndex: 423,
  tvocRaw: 35325.92,
  noxIndex: 1,
  noxRaw: 21638.25,
  boot: 138,
  bootCount: 138,
  ledMode: 'off',
  firmware: '3.6.0',
  model: 'I-9PSL',
});

const outdoorFull = Object.freeze({
  wifi: -42,
  serialno: 'ecda3b1d09d8',
  rco2: 440,
  pm01: 23.17,
  pm02: 35.33,
  pm10: 39.17,
  pm003Count: 3608.5,
  pm02Compensated: 23.72,
  atmp: 9.8,
  atmpCompensated: 6.27,
  rhum: 35,
  rhumCompensated: 51.41,
  tvocIndex: 231.08,
  tvocRaw: 35476.58,
  noxIndex: 1,
  noxRaw: 21618.67,
  boot: 89,
  bootCount: 89,
  firmware: '3.6.0',
  model: 'O-1PST',
});

// After reboot: most sensor fields are null (from python-airgradient fixtures)
const afterBoot = Object.freeze({
  wifi: -59,
  serialno: '84fce602549c',
  rco2: null,
  pm01: null,
  pm02: null,
  pm10: null,
  atmp: null,
  atmpCompensated: null,
  rhum: null,
  rhumCompensated: null,
  tvocIndex: null,
  noxIndex: null,
  boot: 0,
  bootCount: 0,
  firmware: '3.6.0',
  model: 'I-9PSL',
});

// Compensated values can be zero — zero is valid, not missing
// (from python-airgradient current_measures_zero.json)
const zeroCompensated = Object.freeze({
  wifi: -45,
  serialno: '84fce602549c',
  rco2: 400,
  pm01: 5,
  pm02: 10,
  pm10: 12,
  pm02Compensated: 0,
  atmp: 22.5,
  atmpCompensated: 0,
  rhum: 50,
  rhumCompensated: 0,
  tvocIndex: 100,
  noxIndex: 1,
  boot: 10,
  bootCount: 10,
  firmware: '3.6.0',
  model: 'I-9PSL',
});

module.exports = { indoorFull, outdoorFull, afterBoot, zeroCompensated };
