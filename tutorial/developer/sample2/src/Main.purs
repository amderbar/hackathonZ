module Main where

--------------------------------------------------------
--  サンプルソース2
--    HalogenとRoutingを利用して
--    URL Hashに応じた内容を表示する（SPA）
--------------------------------------------------------

-- モジュールのロード
import Prelude
import Data.Foldable             (oneOf)
import Data.Maybe                (Maybe(..))
import Effect                    (Effect)
import Effect.Aff                (Aff)
import Effect.Aff              as Aff
import Halogen                 as Halo
import Halogen.Aff             as HAff
import Halogen.HTML            as Html
import Halogen.HTML.Properties as HProp
import Halogen.VDom.Driver     as HVdom
import Routing.Match           as Match
import Routing.Hash            as Route
import Web.HTML                as Web
import Web.HTML.Window         as Window
import Web.HTML.Location       as Location

main :: Effect Unit
main = HAff.runHalogenAff do
  hash <- Halo.liftEffect $ Web.window >>= Window.location >>= Location.hash
  body <- HAff.awaitBody
  app <- HVdom.runUI haloApp unit body
  routing app.query ChangeHash

-- | ルーティングの設定

-- | 取りうるHash Routeを列挙した型
data RouteHash
    = Index
    | Rakus
derive instance eqRouteHash :: Eq RouteHash

-- ｜ RouteHash の文字列表現
instance showRouteHash :: Show RouteHash where
  show Index = ""
  show Rakus = "#rakus"

-- ｜ 文字列表現から RouteHash を得るパーサー
menuHash :: Match.Match RouteHash
menuHash = oneOf
    [ Rakus <$ Match.lit "rakus"
    , pure Index
    ] <* Match.end

-- ｜ RouteHash パーサーの実行関数
routing :: forall m a
         . (m Unit -> Aff a)
        -> (RouteHash -> Unit -> m Unit)
        -> Aff (Effect Unit)
routing query hashAction =
  Halo.liftEffect $ Route.matches menuHash \_ newHash ->
      Aff.launchAff_ $ query $ Halo.action $ hashAction newHash

-- | UI の設定
-- | 関連データ型
type State = RouteHash
data Query a = ChangeHash RouteHash a
type Message = Unit

-- | コンポーネント定義
haloApp :: Halo.Component Html.HTML Query Unit Message Aff
haloApp =
  Halo.component
    { initialState: const Index
    , render
    , eval
    , receiver    : const Nothing
    }

-- | 表示の定義
render :: State -> Halo.ComponentHTML Query
render state =
  Html.main -- いちばん外側のコンテナ要素
    [ HProp.attr (Html.AttrName "style") "width: 100%; height: 100%;" ]
    -- stateの値（Index or Rakus）によって表示する内容を切り替える
    case state of
      Index -> -- ルートパス「/」にアクセスした場合の表示
        [ Html.h1_ [ Html.text "Sample2" ]
        , Html.text "Hello World!"
        ]
      Rakus -> -- 「/#rakus」にアクセスした場合の表示
        [ Html.h1_ [ Html.text "ここに表示したい値を記述する" ]
        , Html.text "ここに表示したい値を記述する"
        ]

-- | イベントハンドラ
eval :: Query ~> Halo.ComponentDSL State Query Message Aff
eval (ChangeHash newHash next) = do
  Halo.put newHash
  pure next
