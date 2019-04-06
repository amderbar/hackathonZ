module Main where

--------------------------------------------------------
--  サンプルソース4
--    Query Algebraを利用してイベントハンドリングと状態の更新を行う
--------------------------------------------------------

-- モジュールのロード
import Prelude
import Data.Maybe                (Maybe(..))
import Data.String               (null)
import Effect                    (Effect)
import Effect.Aff                (Aff)
import Halogen                 as Halo
import Halogen.Aff             as HAff
import Halogen.HTML            as Html
import Halogen.HTML.Events     as HEvent
import Halogen.HTML.Properties as HProp
import Halogen.VDom.Driver     as HVdom
import Web.HTML                as Web
import Web.HTML.Window         as Window

main :: Effect Unit
main = HAff.runHalogenAff do
  body <- HAff.awaitBody
  HVdom.runUI haloApp unit body

-- | UI の設定
-- | 関連データ型
type State =
  { text1 :: String
  , text2 :: String
  }
data Query a
  = InputText String a
  | PopupValue a
  | SetValue a
type Message = Unit

-- | コンポーネント定義
haloApp :: Halo.Component Html.HTML Query Unit Message Aff
haloApp =
  Halo.component
    { initialState: const initialState
    , render
    , eval
    , receiver    : const Nothing
    }

initialState :: State
initialState =
  { text1 : ""
  , text2 : ""
  }

-- | 表示の定義
render :: State -> Halo.ComponentHTML Query
render state =
  Html.main
    [ HProp.attr (Html.AttrName "style") "width: 100%; height: 100%;" ]
    [ Html.h1_ [ Html.text "Sample4" ]
    , Html.h3_ [ Html.text "フォームの値を取得する" ]
    , Html.p_
      [ Html.text "テキストボックスに値を入力してください"
      , Html.br_
      , Html.text "フォームに値を入力していないときはポップアップは表示されません"
      , Html.br_
      , Html.input
        [ HProp.type_ HProp.InputText
        , HProp.id_ "text1"
        , HProp.value state.text1
        , HEvent.onValueInput (HEvent.input InputText)
        ]
      , Html.input
        [ HProp.type_ HProp.InputButton
        , HProp.value "GET"
        , HEvent.onClick (HEvent.input_ PopupValue)
        ]
      ]
    , Html.h3_ [ Html.text "フォームに値をセットする" ]
    , Html.p_
      [ Html.text "SETボタンを押してください"
      , Html.br_
      , Html.text "フォームに値を入力していないときはポップアップは表示されません"
      , Html.br_
      , Html.input
        [ HProp.type_ HProp.InputButton
        , HProp.value "SET"
        , HEvent.onClick (HEvent.input_ SetValue)
        ]
      , Html.div
        [ HProp.id_ "text2" ]
        [ Html.text state.text2 ]
      ]
    ]

-- | イベントハンドラ
eval :: Query ~> Halo.ComponentDSL State Query Message Aff
-- 要素の値を取得する
eval (InputText inp next) = do
  Halo.modify_ (_ { text1 = inp })
  pure next

-- フォームに値が設定されている場合にポップアップを表示する
eval (PopupValue next) = do
  text1 <- _.text1 <$> Halo.get
  -- ここに条件分岐を記入する
  Halo.liftEffect $ when (not $ null text1) do
    Web.window >>= Window.alert text1
    pure unit
  pure next

-- 要素に値をセットする
eval (SetValue next) = do
  Halo.modify_ (_ { text2 = "Rakus!" })
  pure next
