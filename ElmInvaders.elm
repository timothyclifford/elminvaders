module ElmInvaders where

import ElmInvadersModels exposing (..)

import Time exposing (..)
import Keyboard exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

-- UPDATE

defaultShip : Ship
defaultShip =
  { x = 0
  , y = 0
  }

defaultInvaders : List Invader
defaultInvaders =
  []

defaultState : State
defaultState =
  State StartView defaultShip defaultInvaders 0 3

delta : Signal Time
delta =
    Signal.map inSeconds (fps 30)

input : Signal Input
input =
    Signal.sampleOn delta <|
        Signal.map3
          Input
          Keyboard.space
          Keyboard.arrows
          delta

stepGame : Input -> State -> State
stepGame input state =
  state

gameState : Signal State
gameState =
    Signal.foldp stepGame defaultState input

-- VIEW

startView : Signal.Address Action -> Html
startView address =
  div [ class "start" ]
  [ button [ onClick address StartGame ] [ text "Click me" ]
  ]

gameView : Signal.Address Action -> State -> Html
gameView address state =
  div [] [ text "Ship" ]

endView : Signal.Address Action -> State -> Html
endView address state =
  div [] [ text "End" ]

-- view : Signal.Address Action -> State -> Html
-- view address state =
--   case state.screen of
--     StartView ->
--       startView address
--     GameView ->
--       gameView address state
--     EndView ->
--       endView address state

view : (Int,Int) -> State -> Html
view (w,h) game =
  div [] [ text "Hello" ]
