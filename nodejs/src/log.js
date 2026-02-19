'use strict';

/**
 * Returns a formatted timestamp string: [YYYY-MM-DD HH:MM:SS]
 */
function timestamp() {
  const d = new Date();
  const Y = d.getFullYear();
  const M = String(d.getMonth() + 1).padStart(2, '0');
  const D = String(d.getDate()).padStart(2, '0');
  const h = String(d.getHours()).padStart(2, '0');
  const m = String(d.getMinutes()).padStart(2, '0');
  const s = String(d.getSeconds()).padStart(2, '0');
  return `[${Y}-${M}-${D} ${h}:${m}:${s}]`;
}

module.exports = { timestamp };
