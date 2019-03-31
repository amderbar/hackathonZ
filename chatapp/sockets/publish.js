'use strict';

module.exports = function (socket, io) {
    // 投稿メッセージを送信する
    socket.on('publishEvent', function (data) {
        // 全員に送信
        io.sockets.emit('publishEvent', data);
    });
};
