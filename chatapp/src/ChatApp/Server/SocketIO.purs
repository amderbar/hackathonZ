module ChatApp.Server.SocketIO where

import Prelude
import Effect (Effect)
import Data.Function.Uncurried (Fn3, runFn1, runFn3)
import Node.HTTP          as Node

foreign import data Socket :: Type

foreign import socketServerImpl :: Node.Server -> Effect Node.Server

listen :: Node.Server -> Effect Node.Server
listen server = runFn1 socketServerImpl server
