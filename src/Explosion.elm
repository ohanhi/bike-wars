module Explosion exposing (..)

import Constants exposing (..)
import Math.Vector2 as Vec2 exposing (Vec2, getX, getY, vec2)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Types exposing (Explosion, Trail)


forBike : Vec2 -> Explosion
forBike center =
    { center = center, size = 30.0, ticksLeft = 30 }


toTrail : List Explosion -> Trail
toTrail explosionList =
    let
        left a =
            getX a.center - a.size / 2

        right a =
            getX a.center + a.size / 2

        top a =
            getY a.center - a.size / 2

        bottom a =
            getY a.center + a.size / 2
    in
    List.map
        (\a ->
            [ vec2 (left a) (top a)
            , vec2 (right a) (top a)
            , vec2 (right a) (bottom a)
            , vec2 (left a) (bottom a)
            ]
        )
        explosionList


view : List Explosion -> List (Svg msg)
view =
    List.map explosionForm


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
