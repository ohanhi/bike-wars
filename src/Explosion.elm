module Explosion exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Math.Vector2 as Vec2 exposing (Vec2, vec2, getX, getY)
import Types exposing (Explosion)
import Constants exposing (..)


view : List Explosion -> List (Svg msg)
view =
    List.map explosionForm


explosionForm : Explosion -> Svg msg
explosionForm explosion =
    rect
        [ strokeWidth "0"
        , fill colors.yellow
        , height (toString explosion.size)
        , width (toString explosion.size)
        , x (toString (getX explosion.center - explosion.size / 2))
        , y (toString (getY explosion.center - explosion.size / 2))
        ]
        []
