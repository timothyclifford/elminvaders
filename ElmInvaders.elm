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
screenWidth = 800
screenHeight = 600
screenLeft = -(screenWidth / 2)
screenRight = (screenWidth / 2)
screenTop = (screenHeight / 2)
screenBottom = -(screenHeight / 2)

backgroundColour = (rgba 0 0 0 1)
shipSpeed = 10
shipRadius = 12
shipSize = shipRadius * 2
shipColour = (rgba 255 255 255 1)
bulletSpeed = 10
bulletRadius = 2
bulletSize = bulletRadius * 2
bulletColour = (rgba 255 255 255 1)
invaderRadius = 10
invaderSize = invaderRadius * 2
invaderColour = (rgba 255 255 255 1)
invadersRows = 4
invadersColumns = 8

-- UPDATE

defaultShip : Ship
defaultShip =
  Ship 0

defaultInvaders : List (List Invader)
defaultInvaders =
  [ (List.map (\x -> Invader (screenLeft + (x * 30)) (screenTop - 30) Easy False) [0..invadersColumns])
  , (List.map (\x -> Invader (screenLeft + (x * 30)) (screenTop - 60) Easy False) [0..invadersColumns])
  , (List.map (\x -> Invader (screenLeft + (x * 30)) (screenTop - 90) Easy False) [0..invadersColumns])
  , (List.map (\x -> Invader (screenLeft + (x * 30)) (screenTop - 120) Easy False) [0..invadersColumns])
  ]

defaultBullet : Bullet
defaultBullet =
  Bullet 0 1000

defaultState : State
defaultState =
  State StartView defaultShip defaultInvaders defaultBullet 0 3

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

updateBullet : Input -> State -> Bullet
updateBullet input state =
  let
    bullet = if input.space && (state.bullet.y > screenTop)
      then Bullet state.ship.x (screenBottom + shipRadius)
      else state.bullet
  in
    moveBullet bullet

moveBullet : Bullet -> Bullet
moveBullet bullet =
  { bullet | y = bullet.y + bulletSpeed }

updateInvaders : Input -> State -> Bullet -> List (List Invader)
updateInvaders input state bullet =
  let
    doMove = List.map (\r -> List.map moveInvader r) state.invaders
    doKill = List.map (\r -> List.map (\i -> killInvader i bullet) r) doMove
  in
    doKill

moveInvader : Invader -> Invader
moveInvader invader =
  { invader | y = invader.y - 0.1 }

killInvader : Invader -> Bullet -> Invader
killInvader invader bullet =
  let
    isDead = if invader.dead
      then True
      else (bullet.x < (invader.x + invaderRadius))
        && (bullet.x > (invader.x - invaderRadius))
        && (bullet.y < (invader.y + invaderRadius))
        && (bullet.y > (invader.y - invaderRadius))
  in
    { invader | dead = isDead }

updateGame : Input -> State -> State
updateGame input state =
  let
    --logInput = log "input" input
    --logState = log "state" state
    newShip = updateShip input state
    newBullet = updateBullet input state
    newInvaders = updateInvaders input state newBullet
  in
    { state | ship = newShip
    , invaders = newInvaders
    , bullet = newBullet
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
    |> move (ship.x, (screenBottom + shipRadius))

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
  let
    shapes =
      [ renderBackground (800, 600)
      , renderShip state.ship
      , renderBullet state.bullet ]
    invaders =
      List.concat state.invaders
      |> List.filter (\i -> not i.dead)
      |> List.map renderInvader
    errything = List.append shapes invaders
  in
    collage w h errything
