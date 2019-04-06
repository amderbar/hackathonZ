module Main where

--------------------------------------------------------
--  サンプルソース7
--    Bootstrapを利用してHTML要素の装飾を行う
--------------------------------------------------------

-- モジュールのロード
import Prelude
import Data.Maybe                  (Maybe(..))
import Effect                      (Effect)
import Effect.Aff                  (Aff)
import Halogen                   as Halo
import Halogen.Aff               as HAff
import Halogen.HTML              as Html
import Halogen.HTML.Properties   as HProp
import Halogen.Themes.Bootstrap4 as Boot
import Halogen.VDom.Driver       as HVdom

main :: Effect Unit
main = HAff.runHalogenAff do
  body <- HAff.awaitBody
  HVdom.runUI haloApp unit body

-- | UI の設定
-- | 関連データ型
type State = Unit
data Query a = Query a
type Message = Unit

-- | コンポーネント定義
haloApp :: Halo.Component Html.HTML Query Unit Message Aff
haloApp =
  Halo.component
    { initialState: const unit
    , render
    , eval
    , receiver    : const Nothing
    }

-- | 表示の定義
render :: State -> Halo.ComponentHTML Query
render state =
  Html.main
    [ HProp.attr (Html.AttrName "style") "width: 100%; height: 100%;" ]
    [ Html.h1_ [ Html.text "Sample6" ]
    , Html.input
      [ HProp.type_ HProp.InputButton
      , HProp.class_ (Halo.ClassName "")
      , HProp.value "ボタン1"
      ]
    , Html.input
      [ HProp.type_ HProp.InputButton
      , HProp.class_ (Halo.ClassName "")
      , HProp.value "ボタン2"
      ]
    , Html.input
      [ HProp.type_ HProp.InputButton
      , HProp.class_ (Halo.ClassName "")
      , HProp.value "ボタン3"
      ]
    ]

-- | イベントハンドラ
eval :: Query ~> Halo.ComponentDSL State Query Message Aff
eval (Query next) = pure next
