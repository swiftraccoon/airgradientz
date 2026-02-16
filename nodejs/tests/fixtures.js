'use strict';

const fs = require('node:fs');
const path = require('node:path');

// Load test fixtures from the shared canonical file instead of hardcoding.
const fixturesPath = path.join(__dirname, '..', '..', 'test-fixtures.json');
const fixtures = JSON.parse(fs.readFileSync(fixturesPath, 'utf8'));

const indoorFull = Object.freeze(fixtures.indoorFull);
const outdoorFull = Object.freeze(fixtures.outdoorFull);
const afterBoot = Object.freeze(fixtures.afterBoot);
const zeroCompensated = Object.freeze(fixtures.zeroCompensated);

module.exports = { indoorFull, outdoorFull, afterBoot, zeroCompensated };
