/* global document, exports, require, window */
/* jshint -W097 */
"use strict";

require('smoothscroll-polyfill').polyfill();
var frontend = require('../output/Frontend/index');

var domready = require('domready');

domready(function () {
  frontend.main();
});
