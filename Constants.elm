module Constants exposing (..)

import Math.Vector2 exposing (vec2)
import Types exposing (Line(..))


speedC : Float
speedC =
    0.1


w : Float
w =
    400


h : Float
h =
    300


gameBounds : List Line
gameBounds =
    [ Horizontal ( vec2 0 0, vec2 w 0 )
    , Horizontal ( vec2 0 h, vec2 w h )
    , Vertical ( vec2 0 0, vec2 0 h )
    , Vertical ( vec2 w 0, vec2 w h )
    ]
