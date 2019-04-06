module Main where

------------------------------------------------------
--  サンプルソース1
--    npm start コマンドでアプリケーションを起動する
------------------------------------------------------

-- モジュールのロード
import Prelude
import Data.Maybe                (fromMaybe)
import Data.Int                  (fromString)
import Effect                    (Effect)
import Effect.Console          as Console
import Node.Express.App          (App)
import Node.Express.App        as App
import Node.Express.Response   as Response
import Node.HTTP               as Node
import Node.Process            as Env

-- | サーバの設定
main :: Effect Node.Server
main = do
  -- ポート番号の指定
  port <- normalizePort 3000
  -- 接続を待ち受ける
  App.listenHttp app port (onListening port)
  where
    -- | ログ出力
    onListening :: forall t. Int -> t -> Effect Unit
    onListening port _ =
      Console.log $ "Example app listening on port " <> show port <> "!"

-- | ルーティングの設定
app :: App
app = do
  -- ルートパス「/」にアクセスした場合の処理
  App.get "/"      $ Response.send "Hello, World!"
  -- 「/rakus」にアクセスした場合の処理
  App.get "/rakus" $ Response.send "Hello, Rakus!"

-- | ポート番号を環境変数から取得し、Intに直して返す
-- | 環境変数にPORTがなかったり、Intに直せなかった場合は指定されたデフォルト値を返す
normalizePort :: Int -> Effect Int
normalizePort defaultPort = (parseInt defaultPort) <$> Env.lookupEnv "PORT"
  where
    parseInt d = fromMaybe d <<< ((=<<) fromString)
