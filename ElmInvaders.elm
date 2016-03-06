module ElmInvaders where

import ElmInvadersModels exposing (..)

import Color exposing (..)
import Debug exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
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
  , vx = 0
  , vy = 0
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

updateShip : Input -> State -> Ship
updateShip input state =
  let
    updateX = state.ship.x + 1
    updateY = state.ship.y + 1
    updateVX = state.ship.vx + 1
    updateVY = state.ship.vy + 1
  in
    { x = updateX
    , y = updateY
    , vx = updateVX
    , vy = updateVY
    }

updateInvaders : Input -> State -> List Invader
updateInvaders input state =
  []

updateGame : Input -> State -> State
updateGame input state =
  log "input" input
  { state | ship = (updateShip input state)
  , invaders = (updateInvaders input state)
  }

gameState : Signal State
gameState =
    Signal.foldp updateGame defaultState input

-- VIEW

view : (Int, Int) -> State -> Element
view (w, h) state =
  collage 800 600
  [ rect 100 100
    |> filled (rgba 0 0 0 0.5)
    |> move (state.ship.x, state.ship.y)
  ]
