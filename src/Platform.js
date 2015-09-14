// module Platform

var platform = require("platform");

exports.parse_ = function(str) {
  return platform.parse(str);
};

exports.getPlatform_ = function() {
  return platform;
};
