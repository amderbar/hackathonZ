module Effect.SocketIO.Client where

import Prelude
import Effect (Effect)
import Data.Function.Uncurried (Fn3, runFn1, runFn3)

foreign import data Socket :: Type

type Host = String
type Event = String

type EventHandler a = a -> Effect Unit

foreign import connectImpl :: Host -> Effect Socket

foreign import closeImpl :: Socket -> Effect Unit

foreign import emitImpl :: forall d. Fn3 Socket Event d (Effect Unit)

foreign import onImpl :: forall a. Fn3 Socket Event (EventHandler a) (Effect Unit)


connect :: Host -> Effect Socket
connect host = runFn1 connectImpl host

close :: Socket -> Effect Unit
close socket = runFn1 closeImpl socket

emit :: forall d. Socket -> Event -> d -> Effect Unit
emit socket event dataObj = runFn3 emitImpl socket event dataObj

on :: forall a. Socket -> Event -> (EventHandler a) -> Effect Unit
on socket event callback = runFn3 onImpl socket event callback
