module Constants exposing (..)

import Math.Vector2 exposing (vec2)
import Types exposing (Trail)


bikeSize : Float
bikeSize =
    4


trailSize : Float
trailSize =
    1.0


speedC : Float
speedC =
    0.1


w : Float
w =
    400


h : Float
h =
    300


ticksPerSecond : Int
ticksPerSecond =
    60


gameBounds : Trail
gameBounds =
    [ [ vec2 0 0
      , vec2 w 0
      , vec2 w h
      , vec2 0 h
      , vec2 0 0
      ]
    ]


colors =
    { blue = "#0009a9"
    , red = "#fe5555"
    , green = "#4eee5b"
    , grey = "#222"
    , white = "white"
    , yellow = "yellow"
    }
