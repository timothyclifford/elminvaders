module ElmInvadersModels where

type View = StartView | GameView | EndView
type Action = StartGame | Move | Shoot

type alias Ship =
  { posX: Int
  , posY: Int
  }

type alias Invader =
  { posX: Int
  , posY: Int
  }

type alias State =
  { screen: View
  , ship: Ship
  , invaders: List Invader
  , score: Int
  , lives: Int
  }
