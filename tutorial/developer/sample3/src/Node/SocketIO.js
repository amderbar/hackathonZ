'use strict';

exports.attach = function (server) {
    return function () {
        return require('socket.io')(server, { wsEngine: 'ws' });
    };
};

exports.onConnection = function (server) {
    return function (callback) {
        return function () {
            server.sockets.on('connection', function (socket) {
                callback(socket)();
            });
        };
    };
};

exports.sockets = function (io) {
    return function () {
        return io.sockets;
    };
};

exports.broadcast = function (socket) {
    return function () {
        return socket.broadcast;
    };
};

exports.emitImpl = function (socket, eventName, data) {
    return function () {
        socket.emit(eventName, data);
    };
};
  
exports.onImpl = function (socket, eventName, callback) {
    return function () {
        socket.on(eventName, function(data) {
            callback(data)();
        });
    };
;}
  