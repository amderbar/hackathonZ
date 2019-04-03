module Node.Morgan where

import Prelude                 (Unit)
import Effect                  (Effect)
import Node.Express.Types      (Response, Request)
import Data.Function.Uncurried (Fn3)

foreign import logger :: String -> Fn3 Request Response (Effect Unit) (Effect Unit)
