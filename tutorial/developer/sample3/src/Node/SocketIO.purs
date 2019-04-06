module Node.SocketIO where

import Prelude                 (Unit)
import Effect                  (Effect)
import Data.Function.Uncurried (Fn3, runFn3)
import Node.HTTP               (Server)

foreign import data Socket :: Type

foreign import attach :: Server -> Effect Server

foreign import sockets :: Server -> Effect Socket

foreign import broadcast :: Socket -> Effect Socket

foreign import onConnection :: Server -> (Socket -> Effect Unit) -> Effect Unit

foreign import emitImpl :: forall d. Fn3 Socket Event d (Effect Unit)

foreign import onImpl :: forall a. Fn3 Socket Event (EventHandler a) (Effect Unit)

type EventHandler a = a -> Effect Unit

type Event = String

emit :: forall d. Socket -> Event -> d -> Effect Unit
emit socket event dataObj = runFn3 emitImpl socket event dataObj

on :: forall a. Socket -> Event -> (EventHandler a) -> Effect Unit
on socket event callback = runFn3 onImpl socket event callback
