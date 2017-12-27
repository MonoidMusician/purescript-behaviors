"use strict";

var currentKeys = [];
addEventListener("keydown", function(e) {
  var index = currentKeys.indexOf(e.key);
  if (index < 0) {
    currentKeys.push(e.key);
  }
});
addEventListener("keyup", function(e) {
  var index = currentKeys.indexOf(e.key);
  if (index >= 0) {
    currentKeys.splice(index, 1);
  }
});

exports.withKeysImpl = function (e) {
  return function(sub) {
    return e(function(a) {
      sub({ keys: currentKeys, value: a });
    });
  };
};

exports.downImpl = function(sub) {
  var cb = function(e) {
    sub(e.key);
  };
  addEventListener("keydown", cb);
  return function() {
    removeEventListener("keydown", cb);
  }
};

exports.upImpl = function(sub) {
  var cb = function(e) {
    sub(e.key);
  };
  addEventListener("keyup", cb);
  return function() {
    removeEventListener("keyup", cb);
  }
};
