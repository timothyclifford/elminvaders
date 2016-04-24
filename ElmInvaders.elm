module ElmInvaders where

import ElmInvadersModels exposing (..)
import Array exposing (..)
import Color exposing (..)
import Debug exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Random exposing (..)
import Text exposing (..)
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
shipSpeed = 8
shipRadius = 12
shipSize = shipRadius * 2
shipColour = (rgba 255 255 255 1)
shieldWidth = 75
shieldHeight = 5
shieldColour = (rgba 255 255 255 1)
bulletSpeed = 15
bulletMax = 600
bulletRadius = 4
bulletSize = bulletRadius * 2
bulletColour = (rgba 255 255 255 1)
invaderBulletSpeed = 10
invaderBulletMax = -600
invaderRadius = 15
invaderSize = invaderRadius * 2
invaderColour = (rgba 255 255 255 1)
invaderGapX = 25
invaderGapY = 15
invadersRows = 4
invadersColumns = 8
invadersTotal = invadersRows * invadersColumns
invadersMoveX = 10
invadersMoveY = 5
invadersMoveMax = screenWidth - (invaderSize * 2) - (invadersColumns * (invaderSize + invaderGapX))

-- UPDATE

defaultShip : Ship
defaultShip =
  Ship 0 (screenBottom + shipSize)

defaultShields : List Shield
defaultShields =
  [ Shield -300 (screenBottom + 80) 5
  , Shield -100 (screenBottom + 80) 5
  , Shield 100 (screenBottom + 80) 5
  , Shield 300 (screenBottom + 80) 5
  ]

defaultInvaders : List Invader
defaultInvaders =
  [0..(invadersTotal - 1)]
    |> List.map (\i -> defaultInvader i)

defaultInvader : Float -> Invader
defaultInvader index =
  let
    row = toFloat (floor (index / invadersColumns))
    column = if row == 0 then index else index - (row * invadersColumns)
    paddingLeft = screenLeft + invaderSize
    paddingTop = screenTop - 50 - invaderSize
    x = paddingLeft + (column * (invaderSize + invaderGapX))
    y = paddingTop - (row * (invaderSize + invaderGapY))
  in
    Invader x y row column 0 Right Easy False

defaultBullet : Bullet
defaultBullet =
  Bullet 0 bulletMax

defaultInvaderBullet : Bullet
defaultInvaderBullet =
  Bullet 0 invaderBulletMax

defaultState : State
defaultState =
  State
    defaultShip defaultShields defaultInvaders defaultBullet defaultInvaderBullet 0 3 False

delta : Signal Time
delta =
  Signal.map inSeconds (fps 30)

input : Signal Input
input =
  Signal.sampleOn delta <| Signal.map3 Input Keyboard.space Keyboard.arrows delta

updateShip : Input -> State -> Ship
updateShip input state =
  { x = state.ship.x + toFloat (input.arrows.x * shipSpeed)
  , y = state.ship.y
  }

updateShields : Input -> State -> List Shield
updateShields input state =
  -- Check if bullet within shield
  state.shields
    |> List.map (\s -> updateShieldWithBullet s state.bullet)
    |> List.map (\s -> updateShieldWithBullet s state.invaderBullet)

updateShieldWithBullet : Shield -> Bullet -> Shield
updateShieldWithBullet shield bullet =
  if shield.layers > 0 && didBulletHitShield shield bullet
  then { shield | layers = shield.layers - 1 }
  else shield

updateBullet : Input -> State -> Bullet
updateBullet input state =
  let
    -- Is bullet hitting a shield
    bullet1 = updateBulletWithShields state.shields state.bullet bulletMax
      -- Is bullet hitting an invader
    bullet2 = updateBulletWithInvaders state.invaders bullet1
    -- Is bullet being shot
    bullet3 =
      if input.space && (state.bullet.y > screenTop)
      then Bullet state.ship.x (screenBottom + shipRadius)
      else bullet2
  in
    moveBullet bullet3

moveBullet : Bullet -> Bullet
moveBullet bullet =
  { bullet | y = bullet.y + bulletSpeed }

updateInvaders : Input -> State -> List Invader
updateInvaders input state =
  let
    invaders' = List.map (\i -> isInvaderDead i state.bullet) state.invaders
  in
    if state.steps % 12 == 0
    then List.map moveInvader invaders'
    else invaders'

isInvaderDead : Invader -> Bullet -> Invader
isInvaderDead invader bullet =
  { invader | dead = invader.dead || (bulletHitInvader invader bullet) }

moveInvader : Invader -> Invader
moveInvader invader =
  -- If moving right and at screen edge, change to left
  if
    invader.delta >= invadersMoveMax && invader.direction == Right
  then
    { invader
    | y = invader.y - invadersMoveY
    , delta = 0
    , direction = Left
    }
  -- If moving left and at screen edge, change to right
  else if
    invader.delta >= invadersMoveMax && invader.direction == Left
  then
    { invader
    | y = invader.y - invadersMoveY
    , delta = 0
    , direction = Right
    }
  -- Keep moving
  else
    { invader
    | x = invader.x + (if invader.direction == Right then invadersMoveX else -invadersMoveX)
    , delta = invader.delta + invadersMoveX
    }

updateBulletWithShip : Ship -> Bullet -> Bullet
updateBulletWithShip ship bullet =
  { bullet
  | y = (if didBulletHitShip ship bullet then invaderBulletMax else bullet.y)
  }

didBulletHitShip : Ship -> Bullet -> Bool
didBulletHitShip ship bullet =
  closeTo
    (bullet.x + bulletRadius, bullet.y + bulletRadius)
    (bullet.x - bulletRadius, bullet.y - bulletRadius)
    (ship.x - shipRadius, ship.y - shipRadius)
    (ship.x + shipRadius, ship.y + shipRadius)

updateBulletWithShields : List Shield -> Bullet -> Float -> Bullet
updateBulletWithShields shields bullet reset =
  let
    -- Iterate through all rows then columns, check bullet is within shield
    didHit = List.any (\s -> s.layers > 0 && (didBulletHitShield s bullet)) shields
  in
    { bullet | y = (if didHit then reset else bullet.y) }

didBulletHitShield : Shield -> Bullet -> Bool
didBulletHitShield shield bullet =
  closeTo
    (bullet.x + bulletRadius, bullet.y + bulletRadius)
    (bullet.x - bulletRadius, bullet.y - bulletRadius)
    (shield.x - (shieldWidth / 2), shield.y - shieldHeight)
    (shield.x + (shieldWidth / 2), shield.y + shieldHeight)

updateBulletWithInvaders : List Invader -> Bullet -> Bullet
updateBulletWithInvaders invaders bullet =
  let
    -- Iterate through all rows then columns, check bullet is within invader
    didHit = List.any (\i -> not i.dead && (bulletHitInvader i bullet)) invaders
  in
    { x = bullet.x
    , y = if didHit then bulletMax else bullet.y
    }

bulletHitInvader : Invader -> Bullet -> Bool
bulletHitInvader invader bullet =
  closeTo
    (bullet.x + bulletRadius, bullet.y + bulletRadius)
    (bullet.x - bulletRadius, bullet.y - bulletRadius)
    (invader.x - invaderRadius, invader.y - invaderRadius)
    (invader.x + invaderRadius, invader.y + invaderRadius)

closeTo : (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool
closeTo (ax1, ay1) (ax2, ay2) (bx1, by1) (bx2, by2) =
  (ax1 >= bx1)
  && (ay1 >= by1)
  && (ax2 <= bx2)
  && (ay2 <= by2)

updateInvaderBullet : List Invader -> Input -> State -> Bullet
updateInvaderBullet invaders input state =
  if state.invaderBullet.y > invaderBulletMax
  then moveInvaderBullet state
  else createInvaderBullet invaders input.delta

moveInvaderBullet : State -> Bullet
moveInvaderBullet state =
  let
    bullet1 = updateBulletWithShields state.shields state.invaderBullet invaderBulletMax
    bullet2 = updateBulletWithShip state.ship bullet1
  in
    { bullet2 | y = bullet2.y - invaderBulletSpeed }

createInvaderBullet : List Invader -> Time -> Bullet
createInvaderBullet invaders time =
  let
    firstInColumn = [0..invadersColumns]
      |> List.filterMap (\c -> getFirst invaders c)
    random = getRandom firstInColumn time
  in
    shootBullet random

getFirst : List Invader -> Float -> Maybe Invader
getFirst invaders column =
  List.filter (\i -> i.column == column) invaders
    |> List.sortBy .row
    |> List.reverse
    |> List.head

getRandom : List Invader -> Time -> Maybe Invader
getRandom invaders seed =
  let
    array = Array.fromList invaders
    initialSeed = Random.initialSeed (round (seed * 1000))
    generator = Random.int 0 ((Array.length array) - 1)
    random = fst (Random.generate generator initialSeed)
  in
    Array.get random array

shootBullet : Maybe Invader -> Bullet
shootBullet invader =
  case invader of
    Just invader -> Bullet invader.x invader.y
    Nothing -> Bullet 0 invaderBulletMax

updateGame : Input -> State -> State
updateGame input state =
  let
    ship' = updateShip input state
    shields' = updateShields input state
    invaders' = updateInvaders input state
    bullet' = updateBullet input state
    invaderBullet' = updateInvaderBullet invaders' input state
    lives' = if (didBulletHitShip state.ship state.invaderBullet) then state.lives - 1 else state.lives
    paused' = state.paused
  in
    { state
    | ship = ship'
    , shields = shields'
    , invaders = invaders'
    , bullet = bullet'
    , invaderBullet = invaderBullet'
    , steps = state.steps + 1
    , lives = lives'
    , paused = paused'
    }

gameState : Signal State
gameState =
  Signal.foldp updateGame defaultState input

-- VIEW

renderBackground : (Int, Int) -> Form
renderBackground (w, h) =
  rect (toFloat w) (toFloat h)
    |> filled backgroundColour

livesStyle : Style
livesStyle =
  { typeface = [ "calibri" ]
  , height   = Just 16
  , color    = white
  , bold     = False
  , italic   = False
  , line     = Nothing
  }

renderLives : Int -> Form
renderLives lives =
  Text.style livesStyle (fromString ("SHIPS: " ++ (toString lives)))
    |> centered
    |> toForm
    |> move (-360, 280)

renderShip : Ship -> Form
renderShip ship =
  rect shipSize shipSize
    |> filled shipColour
    |> move (ship.x, ship.y)

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
      , renderLives state.lives
      , renderShip state.ship
      , renderBullet state.bullet
      , renderBullet state.invaderBullet
      ]
    shields = List.map renderShield state.shields
    invaders = state.invaders
      |> List.filter (\i -> not i.dead)
      |> List.map renderInvader
    withShields = List.append shapes shields
    withInvaders = List.append withShields invaders
  in
    collage w h withInvaders
