module Server.Sockets where

import Prelude
import Data.String           as Str
import Effect                  (Effect)
import Effect.Console        as Console
import Node.HTTP               (Server)
import Node.SocketIO         as Sock

-- | Socket.IOの処理
listen :: Server -> Effect Server
listen server = do
  io <- Sock.attach server
  -- 接続
  Sock.onConnection io \socket -> do
    -- 自クライアントに接続イベント（enteringMyselfEvent）を送信する
    Sock.emit socket "enterMyselfEvent" "あなたが接続しました。"
    -- 自クライアント以外に接続イベント（enterOtherEvent）を送信する
    Sock.broadcast socket >>= \sock ->
      Sock.emit sock "enterOtherEvent" "他のクライアントが接続しました。"
    -- メッセージ入力イベント（sendMessageEvent）を受信する
    Sock.on socket "sendMessageEvent" (onSendMessageEvent io)
  pure io

onSendMessageEvent :: Server -> String -> Effect Unit
onSendMessageEvent io d =
  if Str.null d
    then pure unit
    else do
      Console.log ("クライアントの入力値：" <> d)
      -- 全クライアントが受信するメッセージ表示イベント（receiveMessageEvent）を送信する
      Sock.sockets io >>= \sock -> Sock.emit sock "receiveMessageEvent" d
