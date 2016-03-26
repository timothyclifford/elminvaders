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
invaderRadius = 15
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
  [ (List.map (\x -> Invader (defaultInvaderX x) (defaultInvaderY 0) 0 Right Easy False) [0..invadersColumns])
  , (List.map (\x -> Invader (defaultInvaderX x) (defaultInvaderY 1) 0 Right Easy False) [0..invadersColumns])
  , (List.map (\x -> Invader (defaultInvaderX x) (defaultInvaderY 2) 0 Right Easy False) [0..invadersColumns])
  , (List.map (\x -> Invader (defaultInvaderX x) (defaultInvaderY 3) 0 Right Easy False) [0..invadersColumns]) ]

defaultInvaderX : Float -> Float
defaultInvaderX x =
  (screenLeft + invaderSize + (x * (invaderSize * 2)))

defaultInvaderY : Float -> Float
defaultInvaderY y =
  (screenTop - invaderSize - (y * (invaderSize * 2)))

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
  { x = state.ship.x + toFloat (input.arrows.x * shipSpeed) }

updateBullet : Input -> State -> Bullet
updateBullet input state =
  let
    -- Is bullet hitting an invader
    bullet' = hitInvader state
    -- Is bullet being shot
    newBullet =
      if input.space && (state.bullet.y > screenTop)
      then Bullet state.ship.x (screenBottom + shipRadius)
      else bullet'
    -- Move bullet
    movedBullet = moveBullet newBullet
  in
    movedBullet

hitInvader : State -> Bullet
hitInvader state =
  let
    -- Iterate through all rows then columns, check if invader is alive and if bullet within bounds
    didHit = List.any (\r -> List.any (\i -> not i.dead && (within i state.bullet)) r) state.invaders
  in
    { x = state.bullet.x
    , y = if didHit then 10000 else state.bullet.y }

moveBullet : Bullet -> Bullet
moveBullet bullet =
  { bullet | y = bullet.y + bulletSpeed }

updateInvaders : Input -> State -> List (List Invader)
updateInvaders input state =
  state.invaders
    |> List.map (\r -> List.map moveInvader r)
    |> List.map (\r -> List.map (\i -> isInvaderDead i state.bullet) r)

moveInvader : Invader -> Invader
moveInvader invader =
  if invader.delta == 270 && invader.direction == Right then
    { invader | x = invader.x - 1, y = invader.y - 1, delta = 0, direction = Left }
  else if invader.delta == 270 && invader.direction == Left then
    { invader | x = invader.x + 1, y = invader.y - 1, delta = 0, direction = Right }
  else
    { invader
    | x = invader.x + (if invader.direction == Right then 1 else -1)
    , delta = invader.delta + 1 }

isInvaderDead : Invader -> Bullet -> Invader
isInvaderDead invader bullet =
  { invader | dead = invader.dead || (within invader bullet) }

within : Invader -> Bullet -> Bool
within invader bullet =
  (bullet.x < (invader.x + invaderRadius)) &&
  (bullet.x > (invader.x - invaderRadius)) &&
  (bullet.y < (invader.y + invaderRadius)) &&
  (bullet.y > (invader.y - invaderRadius))

updateGame : Input -> State -> State
updateGame input state =
  let
    --logInput = log "input" input
    --logState = log "state" state
    ship' = updateShip input state
    bullet' = updateBullet input state
    invaders' = updateInvaders input state
  in
    { state | ship = ship'
    , invaders = invaders'
    , bullet = bullet' }

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
      state.invaders
        |> List.concat
        |> List.filter (\i -> not i.dead)
        |> List.map renderInvader
    errything = List.append shapes invaders
  in
    collage w h errything
