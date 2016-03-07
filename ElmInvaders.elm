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
  Ship 0

defaultInvaders : List Invader
defaultInvaders =
  []

defaultState : State
defaultState =
  State StartView defaultShip defaultInvaders [] 0 3

delta : Signal Time
delta =
    Signal.map inSeconds (fps 30)

input : Signal Input
input = Signal.sampleOn delta <|
  Signal.map3
    Input
    Keyboard.space
    Keyboard.arrows
    delta

updateShip : Input -> State -> Ship
updateShip input state =
  { x = state.ship.x + toFloat (input.arrows.x * 10)
  }

updateInvaders : Input -> State -> List Invader
updateInvaders input state =
  []

moveBullet : Bullet -> Bullet
moveBullet bullet =
  { x = bullet.x
  , y = bullet.y + 1
  }

updateBullets : Input -> State -> List Bullet
updateBullets input state =
  let
    moreBullets = if input.space then Bullet state.ship.x 0 :: state.bullets else state.bullets
    updatedBullets = List.map moveBullet moreBullets
  in
    updatedBullets

updateGame : Input -> State -> State
updateGame input state =
  let
    logInput = log "input" input
    logState = log "state" state
    newShip = updateShip input state
    newInvaders = updateInvaders input state
    newBullets = updateBullets input state
  in
    { state | ship = newShip
    , invaders = newInvaders
    , bullets = newBullets
    }

gameState : Signal State
gameState =
    Signal.foldp updateGame defaultState input

-- VIEW

renderBullet : Bullet -> Form
renderBullet bullet =
  rect 5 5
    |> filled (rgba 255 255 255 1)
    |> move (bullet.x, bullet.y)

render : (Int, Int) -> State -> Element
render (w, h) state =
  container w h midBottom <|
  collage w h
    [ rect (toFloat w) (toFloat h)
      |> filled (rgba 0 0 0 1)
    , rect 25 25
      |> filled (rgba 255 255 255 1)
      |> moveX state.ship.x
    ]
