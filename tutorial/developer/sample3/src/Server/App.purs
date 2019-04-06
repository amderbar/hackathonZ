module Server.App where

--------------------------------------------------------
--  サンプルソース3
--    Socket.IO を利用してリアルタイム通信を行う
--------------------------------------------------------

-- モジュールのロード
import Prelude
import Node.Express.App                     (App)
import Node.Express.App                   as App
import Node.Express.Middleware.Static       (static)

app :: App
app = do
  -- public/ フォルダ以下の静的ファイルを読み込む
  App.use         (static "public")
