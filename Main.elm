import ElmInvaders exposing (..)
import Window
import Keyboard

main =
    Signal.map2 view Window.dimensions gameState
