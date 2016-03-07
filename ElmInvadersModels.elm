module ElmInvadersModels where

import Time exposing (..)

type View = StartView | GameView | EndView
type Action = StartGame | Move | Shoot
type Breed = Easy | Medium | Hard

type alias Ship =
  { x: Float
  , vx: Float
  , y: Float
  , vy: Float
  }

type alias Invader =
  { x: Float
  , vx: Float
  , y: Float
  , vy: Float
  , breed: Breed
  }

type alias State =
  { view: View
  , ship: Ship
  , invaders: List Invader
  , score: Int
  , lives: Int
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
