module ElmInvadersModels where

import Time exposing (..)

type View = StartView | GameView | EndView
type Action = StartGame | Move | Shoot
type Breed = Easy | Medium | Hard
type Direction = Left | Right

type alias Positioned =
  { x: Float
  , y: Float
  }

type alias Ship =
  { x: Float
  , y: Float
  }

type alias Shield =
  { x: Float
  , y: Float
  , layers: Float
  }

type alias Invader =
  { x: Float
  , y: Float
  , row: Float
  , column: Float
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
  { ship: Ship
  , shields: List Shield
  , invaders: List Invader
  , bullet: Bullet
  , invaderBullet: Bullet
  , steps: Int
  , lives: Int
  , paused: Bool
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
