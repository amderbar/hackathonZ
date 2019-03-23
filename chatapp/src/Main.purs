module Main where

import Prelude
import Data.Array              hiding (null)
import Data.Foldable           as Fold
import Data.Generic.Rep
import Data.Generic.Rep.Show
import Data.Maybe
import Data.String
import Effect                    (Effect)
import Effect                  as Eff
import Effect.Class            as EffC
import Effect.Aff                (Aff)
import Effect.Aff              as Aff
import Halogen                 as Halo
import Halogen.Aff             as HAff
import Halogen.HTML            as Html
import Halogen.HTML.Core       as HCore
import Halogen.HTML.Events     as HEvent
import Halogen.HTML.Properties as HProp
import Halogen.VDom.Driver     as HVdom
import Routing.Match           as Match
import Routing.Hash            as Route
import Web.HTML                as Web
import Web.HTML.Window         as Window
-- import OnsenUI                 as Onsen

main :: Effect Unit
main = HAff.runHalogenAff do
  -- Onsen.onsReady
  body <- HAff.awaitBody
  app <- HVdom.runUI haloApp unit body
  routing app.query ChangeHash

-- Routing

data RouteHash
    = Index
    | Room

derive instance eqRouteHash :: Eq RouteHash
derive instance genericRouteHash :: Generic RouteHash _
instance showRouteHash :: Show RouteHash where
    show = genericShow

routeHref :: RouteHash -> String
routeHref  Index    = ""
routeHref  Room     = "#room"

menuHash :: Match.Match RouteHash
menuHash = Fold.oneOf
    [ Room <$ Match.lit "room"
    , pure Index
    ] <* Match.end

routing :: forall m a. (m Unit -> Aff a) -> (RouteHash -> Unit -> m Unit) -> Aff (Effect Unit)
routing query hashAction = EffC.liftEffect $ Route.matches menuHash \_ newHash ->
      Aff.launchAff_ $ query $ Halo.action $ hashAction newHash

-- Root UI Component

type State =
    { route   :: RouteHash
    , user    :: String
    , message :: String
    , msgs    :: Array String
    }

data Query a
    = ChangeHash RouteHash a
    | CancelSubmit a
    | InputName String a
    | EnterRoom a
    | InputMessage String a
    | PublishMessage a
    | ReceiveMessage String a
    | SaveMemo a
    | ExitRoom a

type Message = Unit

haloApp :: Halo.Component Html.HTML Query Unit Message Aff
haloApp =
  Halo.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
  initialState :: State
  initialState =
    { route   : Index
    , user    : ""
    , message : ""
    , msgs    : []
    }

  render :: State -> Halo.ComponentHTML Query
  render state =
    case state.route of
      Index -> indexHbs state
      Room  -> roomHbs state

  eval :: Query ~> Halo.ComponentDSL State Query Message Aff
  eval (ChangeHash newHash next) = do
    Halo.modify_ (_ { route = newHash })
    pure next

  eval (CancelSubmit next) = pure next

  eval (InputName name next) = do
    Halo.modify_ (_ { user = name })
    pure next

  eval (EnterRoom next) = do
    name <- map (trim <<< _.user) Halo.get
    if null name
      then Halo.liftEffect $ Window.alert "ユーザ名を入力してください。" =<< Web.window
      else Halo.liftEffect $ Route.setHash (routeHref Room)
    pure next

  eval (InputMessage msg next) = do
    Halo.modify_ (_ { message = msg })
    pure next

  eval (PublishMessage next) = do
    msg <- map _.message Halo.get
    Halo.modify_ (_ { message = "" })
    eval (ReceiveMessage msg next)
    -- pure next

  eval (ReceiveMessage msg next) = do
    msgs <- map _.msgs Halo.get
    Halo.modify_ (_ { msgs = msg : msgs })
    pure next

  eval (SaveMemo next) = do
    state <- Halo.get
    let msg  = state.message
        msgs = state.msgs
        user = state.user
    Halo.modify_ (_ { message = "", msgs = (user <> "さんのメモ：" <> msg) : msgs })
    pure next

  eval (ExitRoom next) = do
    Halo.modify_ (_ { user = "", message = "", msgs = [] })
    Halo.liftEffect $ Route.setHash (routeHref Index)
    pure next

-- Page Views

containerHbs route body =
  Html.div
    [ HProp.class_ (HCore.ClassName "container") ]
    [ Html.div
        [ HProp.class_ (HCore.ClassName "row") ]
        [ Html.h1 []
          [ Html.text case route of
            Index -> "Node Chat サンプル"
            Room  -> "Node Chat チャットルーム"
          ]
        ]
    , Html.div
        [ HProp.class_ (HCore.ClassName "row") ]
        [ Html.form
          [ HEvent.onSubmit (HEvent.input_ CancelSubmit) ]
          body
        ]
    ]

indexHbs state =
  containerHbs state.route
    [ Html.div
        [ HProp.class_ (HCore.ClassName "index-user_name") ]
        [ Html.text "ユーザ名"
        , Html.input
            [ HProp.type_ HProp.InputText
            , HProp.id_ "userName"
            , HProp.name "userName"
            , HProp.class_ (HCore.ClassName "index-user_name_text")
            , HProp.value state.user
            , HEvent.onValueInput (HEvent.input InputName)
            ]
        ]
    , Html.div
        [ HProp.class_ (HCore.ClassName "index-enter") ]
        [ Html.input
            [ HProp.type_ HProp.InputButton
            , HProp.id_ "userName"
            , HProp.name "userName"
            , HProp.classes $ map Html.ClassName ["common-button", "index-enter_button"]
            , HProp.value "入室する"
            , HEvent.onClick (HEvent.input_ EnterRoom)
            ]
        ]
    ]

roomHbs state =
  containerHbs state.route
    [ Html.div
      [ HProp.class_ (Html.ClassName "room-login_user") ]
      [ Html.text $ state.user <> "さん"
      , Html.input
        [ HProp.type_ HProp.InputHidden
        , HProp.id_ "userName"
        , HProp.value state.user
        ]
      ]
    , Html.div
      [ HProp.class_ (Html.ClassName "room-message") ]
      [ Html.textarea
        [ HProp.id_ "message"
        , HProp.rows 4
        , HProp.class_ (Html.ClassName "room-message_textarea")
        , HProp.value state.message
        , HEvent.onValueInput $ HEvent.input InputMessage
        ]
      ]
    , Html.div
      [ HProp.class_ (Html.ClassName "room-submit") ]
      [ Html.input
        [ HProp.type_ HProp.InputButton
        , HProp.classes $ map Html.ClassName [ "common-button", "room-publish_button" ]
        , HProp.value "投稿"
        , HEvent.onClick (HEvent.input_ PublishMessage)
        ]
      , Html.input
        [ HProp.type_ HProp.InputButton
        , HProp.classes $ map Html.ClassName [ "common-button", "room-memo_button" ]
        , HProp.value "メモ"
        , HEvent.onClick (HEvent.input_ SaveMemo)
        ]
      ]
    , Html.div
      [ HProp.class_ (Html.ClassName "room-thread") ]
      [ Html.div
        [ HProp.id_ "thread" ]
        $ map (\m -> Html.p_ [ Html.text m ]) state.msgs
      ]
    , Html.div
      [ HProp.class_ (Html.ClassName "room-exit") ]
      [ Html.input
        [ HProp.type_ HProp.InputButton
        , HProp.classes $ map Html.ClassName [ "common-button", "room-exit_button" ]
        , HProp.value "退室する"
        , HEvent.onClick (HEvent.input_ ExitRoom)
        ]
      ]
    ]
