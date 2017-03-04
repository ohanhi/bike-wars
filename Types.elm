module Types exposing (..)

import Math.Vector2 exposing (Vec2)
import Direction exposing (Direction)


type Line
    = Horizontal ( Vec2, Vec2 )
    | Vertical ( Vec2, Vec2 )


type alias Bike =
    { position : Vec2
    , trail : List Vec2
    , collided : Bool
    , direction : Direction
    }
