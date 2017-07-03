module Direction exposing (..)

import Types exposing (Direction(..))


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
