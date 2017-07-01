module Explosion exposing (..)

import Constants exposing (..)
import Math.Vector2 as Vec2 exposing (Vec2, getX, getY, vec2)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Types exposing (..)


forBike : Vec2 -> Explosion
forBike center =
    { center = center, size = 30.0, ticksLeft = 15 }


toObstacle : Explosion -> Obstacle
toObstacle explosion =
    let
        ( cx, cy ) =
            Vec2.toTuple explosion.center

        r =
            explosion.size / 2
    in
    { w = cx - r
    , e = cx + r
    , n = cy - r
    , s = cy + r
    }


view : List Explosion -> List (Svg msg)
view =
    List.reverse
        >> List.map explosionForm


update : List Explosion -> List Explosion
update =
    List.map updateOne >> List.filter (.ticksLeft >> (/=) 0)


updateOne : Explosion -> Explosion
updateOne explosion =
    { explosion | ticksLeft = explosion.ticksLeft - 1 }


explosionForm : Explosion -> Svg msg
explosionForm { size, ticksLeft, center } =
    let
        strokeW =
            toFloat (ticksLeft % 10) * 0.05 * size
    in
    rect
        [ strokeWidth (toString strokeW)
        , stroke colors.red
        , fill colors.yellow
        , height (toString (size - strokeW))
        , width (toString (size - strokeW))
        , x (toString (getX center - size / 2 + strokeW / 2))
        , y (toString (getY center - size / 2 + strokeW / 2))
        ]
        []
