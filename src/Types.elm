module Types exposing (..)

import Math.Vector2 exposing (Vec2)
import Direction exposing (Direction)
import Keyboard.Extra exposing (Key)


type Line
    = Horizontal ( Vec2, Vec2 )
    | Vertical ( Vec2, Vec2 )


type alias Trail =
    List (List Vec2)


type alias Controls =
    { left : Key
    , right : Key
    , up : Key
    }


type alias Bike =
    { position : Vec2
    , trail : Trail
    , collided : Bool
    , direction : Direction
    , color : String
    , controls : Controls
    }


type alias Explosion =
    { center : Vec2
    , size : Float
    , ticksLeft : Int
    }
