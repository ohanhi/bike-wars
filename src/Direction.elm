module Direction exposing (..)


type Direction
    = North
    | East
    | South
    | West


turnRight : Direction -> Direction
turnRight dir =
    case dir of
        North ->
            East

        East ->
            South

        South ->
            West

        West ->
            North


turnLeft : Direction -> Direction
turnLeft =
    turnRight << turnRight << turnRight
