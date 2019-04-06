'use strict';

exports.connect = function (url) {
  return function () {
    return require('socket.io-client')(url);
  };
}

exports.close = function (socket) {
  return function () {
    return socket.close();
  };
}
