module Types exposing (..)

import Direction exposing (Direction)
import Keyboard.Extra exposing (Key)
import Math.Vector2 exposing (Vec2)


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


type alias Obstacle =
    { n : Float
    , e : Float
    , s : Float
    , w : Float
    }
