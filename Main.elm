import ElmInvaders exposing (initState, update, view)
import StartApp.Simple exposing (start)

main =
  start
    { model = initState
    , update = update
    , view = view
    }
