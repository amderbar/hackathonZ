'use strict';

module.exports = function (socket) {
    // 退室メッセージをクライアントに送信する
    socket.on('exitEvent', function (data) {
        // 自分以外に送信
        socket.broadcast.emit('exitEvent', data);
    });
};
