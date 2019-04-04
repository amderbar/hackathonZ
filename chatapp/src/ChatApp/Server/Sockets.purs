module ChatApp.Server.Sockets where

import Prelude
import Effect                  (Effect)
import Node.HTTP               (Server)
import Node.SocketIO         as Sock

listen :: Server -> Effect Server
listen server = do
  io <- Sock.attach server
  Sock.onConnection io \socket -> do
    Sock.on socket "publishEvent" (onPublishEvent io)
    Sock.on socket "enterEvent" (onEnterEvent socket)
    Sock.on socket "exitEvent" (onExitEvent socket)
  pure io

onPublishEvent :: forall t. Server -> t -> Effect Unit
onPublishEvent io d =
  Sock.sockets io >>= \sock -> Sock.emit sock "publishEvent" d

onEnterEvent :: forall t. Sock.Socket -> t -> Effect Unit
onEnterEvent socket d =
  Sock.broadcast socket >>= \sock -> Sock.emit sock "enterEvent" d

onExitEvent :: forall t. Sock.Socket -> t -> Effect Unit
onExitEvent sock d = Sock.emit sock "exitEvent" d
