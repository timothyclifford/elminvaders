module ElmInvadersModels where

import Time exposing (..)

type View = StartView | GameView | EndView
type Action = StartGame | Move | Shoot
type Breed = Easy | Medium | Hard
type Direction = Left | Right

type alias Ship =
  { x: Float
  }

type alias Shield =
  { x: Float
  , y: Float
  , layers: Float
  }

type alias Invader =
  { x: Float
  , y: Float
  , delta: Float
  , direction: Direction
  , breed: Breed
  , dead: Bool
  }

type alias Bullet =
  { x: Float
  , y: Float
  }

type alias State =
  { view: View
  , ship: Ship
  , shields: List Shield
  , invaders: List (List Invader)
  , bullet: Bullet
  , invaderBullets: List Bullet
  , score: Int
  , lives: Int
  , steps: Int
  }

type alias KeyboardInput =
  { x: Int
  , y: Int
  }

type alias Input =
  { space: Bool
  , arrows: KeyboardInput
  , delta: Time
  }
