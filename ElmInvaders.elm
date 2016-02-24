module ElmInvaders where

import ElmInvadersModels exposing (..)

import Color exposing (..)
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

view : (Int,Int) -> State -> Element
view (w,h) game =
  collage 800 600
  [ rect 100 100
      |> filled (rgba 0 0 0 0.5)
  ]
