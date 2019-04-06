module Node.SocketIO.Client
    ( module Node.SocketIO
    , Host
    , connect
    , close
    ) where

import Prelude           (Unit)
import Effect            (Effect)
import Node.SocketIO     (Socket, Event, EventHandler, on, emit)

type Host = String

foreign import connect :: Host -> Effect Socket

foreign import close :: Socket -> Effect Unit
