'use strict';

exports.connectImpl = function (url) {
  return function () {
    const s = require('socket.io-client');
    return s(url);
  };
}

exports.closeImpl = function (socket) {
  return function () {
    return socket.close();
  };
}

exports.emitImpl = function (socket, eventName, data) {
  return function () {
    socket.emit(eventName, data);
  };
}

exports.onImpl = function (socket, eventName, callback) {
  return function () {
    socket.on(eventName, function(data) {
      callback(data)();
    });
  };
}
