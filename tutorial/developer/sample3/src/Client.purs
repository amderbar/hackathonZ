module Client where

import Prelude
import Data.Array                ((:))
import Data.Maybe                (Maybe(..), fromMaybe)
import Effect                    (Effect)
import Effect.Aff                (Aff)
import Halogen                 as Halo
import Halogen.Aff             as HAff
import Halogen.HTML            as Html
import Halogen.HTML.Events     as HEvent
import Halogen.HTML.Properties as HProp
import Halogen.VDom.Driver     as HVdom
import Node.SocketIO.Client    as SockIO
import Web.HTML                as Web
import Web.HTML.Window         as Window
import Web.HTML.Location       as Location

main :: Effect Unit
main = HAff.runHalogenAff do
  body <- HAff.awaitBody
  HVdom.runUI haloApp unit body

-- Root UI Component

type State =
    { socket  :: Maybe SockIO.Socket
    , msgs    :: Array String
    }

data Query a
    = ConnectSocket a
    | SendMessage a
    | ReceiveMessage String a

type Message = Unit

haloApp :: Halo.Component Html.HTML Query Unit Message Aff
haloApp =
  Halo.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just (Halo.action ConnectSocket)
    , finalizer  : Nothing
    }
  where
  initialState :: State
  initialState =
    { socket  : Nothing
    , msgs    : []
    }

  render :: State -> Halo.ComponentHTML Query
  render state =
    Html.main
      [ HProp.attr (Html.AttrName "style") "width: 100%; height: 100%;" ]
      [ Html.h1_ [ Html.text "Sample3" ]
      , Html.input
        [ HProp.type_ HProp.InputButton
        , HProp.value "メッセージを入力する"
        , HEvent.onClick (HEvent.input_ SendMessage)
        ]
      , Html.div [ HProp.id_ "thread" ]
        -- 画面上にメッセージを表示
        $ map (\m -> Html.p_ [ Html.text m ]) state.msgs
      ]

  eval :: Query ~> Halo.ComponentDSL State Query Message Aff
  eval (ConnectSocket next) = do
    -- socket.ioの処理開始
    sock <- subscribeSocketIO
    Halo.modify_ (_ { socket = Just sock })
    pure next

  eval (SendMessage next) = do
    win <- Halo.liftEffect Web.window
    st <- Halo.get
    case st.socket of
      (Just sock) -> Halo.liftEffect do
        -- メッセージを入力する
        message <- win # Window.prompt "メッセージを入力してください。\nこのメッセージはすべてのクライアントに送信されます。"
        -- メッセージ入力イベント（sendMessageEvent）を送信する
        SockIO.emit sock "sendMessageEvent" $ fromMaybe "" message
      Nothing     -> pure unit
    pure next

  eval (ReceiveMessage msg next) = do
    msgs <- _.msgs <$> Halo.get
    -- 受信したメッセージを状態に追加
    Halo.modify_ (_ { msgs = msg : msgs })
    pure next

-- socket.io
subscribeSocketIO :: Halo.ComponentDSL State Query Message Aff SockIO.Socket
subscribeSocketIO = do
  host <- Halo.liftEffect $ Web.window >>= Window.location >>= Location.host
  socket <- Halo.liftEffect $ SockIO.connect ("http://" <> host)
  -- 自クライアントの接続イベント（enterMyselfEvent）を受信する
  Halo.subscribe $ Halo.eventSource (attach socket "enterMyselfEvent")    onReceiveEvent
  -- 自クライアント以外の接続イベント（enterOtherEvent）を受信する
  Halo.subscribe $ Halo.eventSource (attach socket "enterOtherEvent")     onReceiveEvent
  -- メッセージ表示イベント（receiveMessageEvent）を受信する
  Halo.subscribe $ Halo.eventSource (attach socket "receiveMessageEvent") onReceiveEvent
  pure socket
  where
    attach :: forall a. SockIO.Socket -> SockIO.Event -> (a -> Effect Unit) -> Effect Unit
    attach sock event = SockIO.on sock event

    onReceiveEvent :: String -> Maybe (Query Halo.SubscribeStatus)
    onReceiveEvent d = Just $ ReceiveMessage d Halo.Listening
