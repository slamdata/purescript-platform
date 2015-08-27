// module Platform

var platform = require("platform");

exports.parse = function(str) {
  return platform.parse(str);
};

exports.getPlatform = function() {
  return platform;
};

exports.platformInfo = function(p) {
  return p;
};

exports._toString = function(p) {
  return p.toString();
};
