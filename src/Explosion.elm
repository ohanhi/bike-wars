module Explosion exposing (explosionForm, forBike, toObstacle, update, updateOne, view)

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
        { x, y } =
            Vec2.toRecord explosion.center

        r =
            explosion.size / 2
    in
    { w = x - r
    , e = x + r
    , n = y - r
    , s = y + r
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
            toFloat (modBy 10 ticksLeft) * 0.05 * size
    in
    rect
        [ strokeWidth (String.fromFloat strokeW)
        , stroke colors.red
        , fill colors.yellow
        , height (String.fromFloat (size - strokeW))
        , width (String.fromFloat (size - strokeW))
        , x (String.fromFloat (getX center - size / 2 + strokeW / 2))
        , y (String.fromFloat (getY center - size / 2 + strokeW / 2))
        ]
        []
