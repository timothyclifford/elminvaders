module ElmInvadersModels where

import Time exposing (..)

type View = StartView | GameView | EndView
type Action = StartGame | Move | Shoot
type Breed = Easy | Medium | Hard

type alias Ship =
  { x: Int
  , y: Int
  }

type alias Invader =
  { x: Int
  , y: Int
  , breed: Breed
  }

type alias State =
  { screen: View
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
