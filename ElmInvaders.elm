module ElmInvaders where

import Html exposing (..)
import Html.Attributes exposing (...)
import Html.Events exposing (...)

-- MODEL

type alias Model = {}

-- UPDATE

type Action = Move | Shoot

update : Action -> Model -> Model
update action model =
  case action of
    Move ->
      model
    Shoot ->
      model

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div [] []
