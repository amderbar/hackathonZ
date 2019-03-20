module Main where

import Prelude
import Data.Maybe
import Effect                  as Eff
import Halogen                 as Halo
import Halogen.Aff             as HAff
import Halogen.HTML            as Html
-- import Halogen.HTML.Events     as HEvent
-- import Halogen.HTML.Properties as HProp
import Halogen.VDom.Driver     as HVdom
-- import OnsenUI                 as Onsen

main :: Eff.Effect Unit
main = HAff.runHalogenAff do
  -- Onsen.onsReady
  body <- HAff.awaitBody
  HVdom.runUI haloApp unit body
  -- app <- HVdom.runUI haloApp unit body
  -- routing app.query ChangeHash


-- Root UI Component

type State = Unit

data Query a = Query a

type Message = Unit

haloApp :: forall m. Halo.Component Html.HTML Query Unit Message m
haloApp =
  Halo.component
    { initialState: const unit
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: State -> Halo.ComponentHTML Query
  render state =
    Html.div
      []
      [ Html.text "Hello World"
      -- [ Onsen.onsButton [] [ Html.text "Click Me" ]
      ]

  eval :: Query ~> Halo.ComponentDSL State Query Message m
  eval (Query next) = pure next
