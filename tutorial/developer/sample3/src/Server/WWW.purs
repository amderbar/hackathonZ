module Server.WWW where

-- モジュールのロード
import Prelude
import Data.Maybe                (fromMaybe)
import Data.Int                  (fromString)
import Effect                    (Effect)
import Effect.Console          as Console
import Node.Express.App        as Express
import Node.HTTP               as Node
import Node.Process            as Env
import Server.App              as App
import Server.Sockets          as Sock

-- | サーバの設定
main :: Effect Node.Server
main = do
  -- ポートの設定をの取得
  port <- normalizePort 3000
  -- サーバオブジェクトを生成する
  server <- Express.listenHttp App.app port (onListening port) -- on connection
  -- Socket.IOの処理
  Sock.listen server
  -- TODO: on server "error" onError
  -- TODO: on server "listening" onListening

-- | ポート番号を環境変数から取得し、Intに直して返す
-- | 環境変数にPORTがなかったり、Intに直せなかった場合は指定されたデフォルト値を返す
normalizePort :: Int -> Effect Int
normalizePort defaultPort = (parseInt defaultPort) <$> Env.lookupEnv "PORT"
  where
    parseInt d = fromMaybe d <<< ((=<<) fromString)

onListening :: forall t. Int -> t -> Effect Unit
onListening port _ =
  -- ログ出力
  Console.log $ "Example app listening on port " <> show port <> "!"
