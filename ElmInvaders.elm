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

-- SETTINGS

backgroundColour = (rgba 0 0 0 1)
shipSpeed = 10
shipSize = 25
shipColour = (rgba 255 255 255 1)
bulletSpeed = 1
bulletSize = 2
bulletColour = (rgba 255 255 255 1)
invaderSize = 20
invaderColour = (rgba 255 255 255 1)
invadersRows = 5
invadersColumns = 8

-- UPDATE

defaultShip : Ship
defaultShip =
  Ship 0

defaultInvaders : List (List Invader)
defaultInvaders =
  let
    seedX = 0
    seedY = 0
    invaders =
      [ invadersRow 0
      , invadersRow 20
      , invadersRow 40
      , invadersRow 60
      ]
  in
    invaders

invadersRow : Float -> List Invader
invadersRow y =
  List.map (\x -> Invader (x * 20) y Easy) [0..invadersColumns]

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
  { x = state.ship.x + toFloat (input.arrows.x * shipSpeed)
  }

updateInvaders : Input -> State -> List (List Invader)
updateInvaders input state =
  List.map (\r -> List.map moveInvader r) state.invaders

moveInvader : Invader -> Invader
moveInvader invader =
  { x = invader.x
  , y = invader.y + 0.1
  , breed = invader.breed
  }

updateBullets : Input -> State -> List Bullet
updateBullets input state =
  let
    -- If player is pressing space, add a bullet to collection
    bullets = if input.space then Bullet state.ship.x 0 :: state.bullets else state.bullets
    -- Move all bullets
    updatedBullets = List.map moveBullet bullets
  in
    updatedBullets

moveBullet : Bullet -> Bullet
moveBullet bullet =
  { x = bullet.x
  , y = bullet.y + bulletSpeed
  }

-- input: { space = False, arrows = { x = 0, y = 0 }, delta = 0.253 }
-- state: { view = StartView, ship = { x = 0 }, invaders = [], bullets = [], score = 0, lives = 3 }
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

renderBackground : (Int, Int) -> Form
renderBackground (w, h) =
  rect (toFloat w) (toFloat h)
    |> filled backgroundColour

renderShip : Ship -> Form
renderShip ship =
  rect shipSize shipSize
    |> filled shipColour
    |> moveX ship.x

renderBullet : Bullet -> Form
renderBullet bullet =
  rect bulletSize bulletSize
    |> filled bulletColour
    |> move (bullet.x, bullet.y)

renderInvader : Invader -> Form
renderInvader invader =
  rect invaderSize invaderSize
    |> filled invaderColour
    |> move (invader.x, invader.y)

render : (Int, Int) -> State -> Element
render (w, h) state =
  container w h midBottom <|
  collage w h
    (List.append
      [ renderBackground (w, h)
      , renderShip state.ship
      ]
      (List.append (List.map renderBullet state.bullets) (List.map renderInvader state.invaders)))
