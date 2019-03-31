'use strict';

module.exports = function (socket) {
    // 入室メッセージをクライアントに送信する
    socket.on('enterEvent', function (data) {
        // 自分以外に送信
        socket.broadcast.emit('enterEvent', data);
    });
};
