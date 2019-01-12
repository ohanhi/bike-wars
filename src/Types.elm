module Types exposing (Bike, Controls, Direction(..), Explosion, Line(..), Obstacle, Trail, Weapon(..))

import Keyboard exposing (Key)
import Math.Vector2 exposing (Vec2)


type Direction
    = North
    | East
    | South
    | West


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
    , weapon : Weapon
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


type Weapon
    = MegaBlaster { used : Bool }
    | AssaultBazooka { shotsLeft : Int }
    | Afterburner { ticksLeft : Int }
