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
shieldWidth = 100
shieldHeight = 5
shieldColour = (rgba 255 255 255 1)
bulletSpeed = 15
bulletRadius = 4
bulletSize = bulletRadius * 2
bulletColour = (rgba 255 255 255 1)
invaderRadius = 15
invaderSize = invaderRadius * 2
invaderColour = (rgba 255 255 255 1)
invaderGapX = 25
invaderGapY = 15
invadersRows = 4
invadersColumns = 8
invadersMoveX = 5
invadersMoveY = 2
invadersMoveMax = screenWidth - (invaderSize * 2) - (invadersColumns * (invaderSize + invaderGapX))

-- UPDATE

defaultShip : Ship
defaultShip =
  Ship 0

defaultShields : List Shield
defaultShields =
  [ Shield -200 (screenBottom + 80) 5
  , Shield 0 (screenBottom + 80) 5
  , Shield 200 (screenBottom + 80) 5 ]

defaultInvaders : List (List Invader)
defaultInvaders =
  [0..invadersRows]
    |> List.map (\y -> (List.map (\x -> defaultInvader x y) [0..invadersColumns]))

defaultInvader : Float -> Float -> Invader
defaultInvader x y =
  let
    x = (screenLeft + invaderSize + (x * (invaderSize + invaderGapX)))
    y = (screenTop - 50 - invaderSize - (y * (invaderSize + invaderGapY)))
  in
    Invader x y 0 Right Easy False

defaultBullet : Bullet
defaultBullet =
  Bullet 0 1000

defaultInvaderBullets : List Bullet
defaultInvaderBullets =
  []

defaultState : State
defaultState =
  State
    StartView
    defaultShip
    defaultShields
    defaultInvaders
    defaultBullet
    defaultInvaderBullets
    0
    3
    0

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

updateShields : Input -> State -> List Shield
updateShields input state =
  -- Check if bullet within shield
  state.shields
    |> List.map (\s -> playerDidShoot s state.bullet)

playerDidShoot : Shield -> Bullet -> Shield
playerDidShoot shield bullet =
  let
    result = bulletHitShield shield bullet
    --log' = log "result" result
  in
    if result
    then { shield | layers = shield.layers - 1 }
    else shield

bulletHitShield : Shield -> Bullet -> Bool
bulletHitShield shield bullet =
  (shield.layers > 0)
  && ((bullet.x - bulletRadius) <= (shield.x + (shieldWidth / 2)))
  && ((bullet.x + bulletRadius) >= (shield.x - (shieldWidth / 2)))
  && ((bullet.y - bulletRadius) <= (shield.y + (shieldHeight)))
  && ((bullet.y + bulletRadius) >= (shield.y - (shieldHeight)))

updateBullet : Input -> State -> Bullet
updateBullet input state =
  let
    -- Is bullet hitting a shield
    bullet1 = hitShield state.shields state.bullet
      -- Is bullet hitting an invader
    bullet2 = hitInvader state.invaders bullet1
    -- Is bullet being shot
    bullet3 =
      if input.space && (state.bullet.y > screenTop)
      then Bullet state.ship.x (screenBottom + shipRadius)
      else bullet2
    -- Move bullet
    movedBullet = moveBullet bullet3
  in
    movedBullet

hitShield : List Shield -> Bullet -> Bullet
hitShield shields bullet =
  let
    -- Iterate through all rows then columns, check bullet is within shield
    didHit = List.any (\s -> (bulletHitShield s bullet)) shields
  in
    { x = bullet.x
    , y = if didHit then 10000 else bullet.y }

hitInvader : List (List Invader) -> Bullet -> Bullet
hitInvader invaders bullet =
  let
    -- Iterate through all rows then columns, check bullet is within invader
    didHit = List.any (\r -> List.any (\i -> (within i bullet)) r) invaders
  in
    { x = bullet.x
    , y = if didHit then 10000 else bullet.y }

moveBullet : Bullet -> Bullet
moveBullet bullet =
  { bullet | y = bullet.y + bulletSpeed }

updateInvaders : Input -> State -> List (List Invader)
updateInvaders input state =
  let
    deadInvaders = List.map (\r -> List.map (\i -> isInvaderDead i state.bullet) r) state.invaders
    invaders' =
      if state.steps % 1 == 0
      then List.map (\r -> List.map moveInvader r) deadInvaders
      else deadInvaders
  in
    invaders'

moveInvader : Invader -> Invader
moveInvader invader =
  -- If moving right and at screen edge, change to left
  if invader.delta >= invadersMoveMax && invader.direction == Right then
    { invader
    | x = invader.x - invadersMoveX
    , y = invader.y - invadersMoveY
    , delta = 0, direction = Left }
  -- If moving left and at screen edge, change to right
  else if invader.delta >= invadersMoveMax && invader.direction == Left then
    { invader
    | x = invader.x + invadersMoveX
    , y = invader.y - invadersMoveY
    , delta = 0, direction = Right }
  -- Keep moving
  else
    { invader
    | x = invader.x + (if invader.direction == Right then invadersMoveX else -invadersMoveX)
    , delta = invader.delta + invadersMoveX }

isInvaderDead : Invader -> Bullet -> Invader
isInvaderDead invader bullet =
  { invader | dead = invader.dead || (within invader bullet) }

within : Invader -> Bullet -> Bool
within invader bullet =
  (not invader.dead)
  && (bullet.x <= (invader.x + invaderRadius))
  && (bullet.x >= (invader.x - invaderRadius))
  && (bullet.y <= (invader.y + invaderRadius))
  && (bullet.y >= (invader.y - invaderRadius))

updateGame : Input -> State -> State
updateGame input state =
  let
    --logInput = log "input" input
    --logState = log "state" state
    ship' = updateShip input state
    bullet' = updateBullet input state
    shields' = updateShields input state
    invaders' = updateInvaders input state
  in
    { state
    | ship = ship'
    , shields = shields'
    , invaders = invaders'
    , bullet = bullet'
    , steps = state.steps + 1 }

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
    |> move (ship.x, (screenBottom + shipSize))

renderShield : Shield -> Form
renderShield shield =
  rect shieldWidth shield.layers
    |> filled shieldColour
    |> move (shield.x, (shield.y))

renderBullet : Bullet -> Form
renderBullet bullet =
  circle bulletRadius
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
      [ renderBackground (screenWidth, screenHeight)
      , renderShip state.ship
      , renderBullet state.bullet ]
    shields = List.map renderShield state.shields
    invaders =
      state.invaders
        |> List.concat
        |> List.filter (\i -> not i.dead)
        |> List.map renderInvader
    invaderBullets = List.map renderBullet state.invaderBullets
    withShields = List.append shapes shields
    withInvaders = List.append withShields invaders
  in
    collage w h withInvaders
