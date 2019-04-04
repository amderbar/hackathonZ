module ChatApp.Server.WWW where

import Prelude
import Data.Maybe                (fromMaybe)
import Data.Int                  (fromString)
import Effect                    (Effect)
import Effect.Console          as Console
import Node.Express.App        as Express
import Node.HTTP               as Node
import Node.Process            as Env
import ChatApp.Server.App      as App
import ChatApp.Server.Sockets  as Sock

main :: Effect Node.Server
main = do
  port <- normalizePort 3000
  server <- Express.listenHttp App.app port (onListening port) -- on connection
  Sock.listen server
  -- TODO: listen as socketIo server
  -- TODO: on server "error" onError
  -- TODO: on server "listening" onListening

normalizePort :: Int -> Effect Int
normalizePort defaultPort = (parseInt defaultPort) <$> Env.lookupEnv "PORT"
  where
    parseInt d = fromMaybe d <<< ((=<<) fromString)

-- TODO: onError err = undefind
onListening :: forall t. Int -> t -> Effect Unit
onListening port _ =
  -- TODO: debug $ "Listening on port" <> show port
  Console.log $ "Chat app listening on port " <> show port <> "!"
