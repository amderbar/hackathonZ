module Node.BodyParser where

import Prelude                 (Unit)
import Effect                  (Effect)
import Node.Express.Types      (Response, Request)
import Data.Function.Uncurried (Fn3)

foreign import json :: Fn3 Request Response (Effect Unit) (Effect Unit)
-- parser option should be able to passed, like this...
-- foreign import json :: forall op. { | op } -> Fn3 Request Response (Effect Unit) (Effect Unit)
