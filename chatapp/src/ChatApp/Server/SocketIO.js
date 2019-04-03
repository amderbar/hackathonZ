'use strict';

exports.socketServerImpl = function (server) {
    return function () {
        require('../../sockets')(server); 
        return server;
    }
};
