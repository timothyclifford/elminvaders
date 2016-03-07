import ElmInvaders exposing (..)
import Window
import Keyboard

main =
    Signal.map2 render Window.dimensions gameState
