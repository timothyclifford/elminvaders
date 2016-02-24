module ElmInvaders where

import ElmInvadersModels exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

-- UPDATE

initShip : Ship
initShip =
  { posX = 0
  , posY = 0
  }

initInvaders : List Invader
initInvaders = []

initState : State
initState = State StartView initShip initInvaders 0 3

update : Action -> State -> State
update action state =
  case action of
    StartGame ->
      { state | screen = GameView }
    Move ->
      state
    Shoot ->
      state

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

view : Signal.Address Action -> State -> Html
view address state =
  case state.screen of
    StartView ->
      startView address
    GameView ->
      gameView address state
    EndView ->
      endView address state
