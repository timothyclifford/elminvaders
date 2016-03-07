module ElmInvadersModels where

import Time exposing (..)

type View = StartView | GameView | EndView
type Action = StartGame | Move | Shoot
type Breed = Easy | Medium | Hard

type alias Ship =
  { x: Float
  }

type alias Invader =
  { x: Float
  , y: Float
  , breed: Breed
  }

type alias Bullet =
  { x: Float
  , y: Float
  }

type alias State =
  { view: View
  , ship: Ship
  , invaders: List Invader
  , bullets: List Bullet
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
