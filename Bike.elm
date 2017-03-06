module Bike exposing (..)

import Keyboard.Extra
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Math.Vector2 as Vec2 exposing (Vec2, vec2, getX, getY)
import Types exposing (Bike)
import Direction exposing (..)
import BikePhysics exposing (..)


initBike : Bike
initBike =
    { position = vec2 20 20
    , trail = [ vec2 20 20 ]
    , collided = False
    , direction = East
    , color = "yellow"
    }


move : Float -> Bike -> Bike
move diff bike =
    { bike
        | position =
            if bike.collided then
                bike.position
            else
                computePosition bike.direction bike.position diff
        , collided =
            isCollision bike.direction bike.position bike.trail
    }


turn : Keyboard.Extra.Key -> Bike -> Bike
turn key bike =
    { bike
        | direction = computeDirection bike.direction key
        , trail = bike.position :: bike.trail
    }


view : Bike -> List (Svg msg)
view bike =
    let
        bikeSize =
            6

        posX =
            getX bike.position

        posY =
            getY bike.position

        rotation =
            case bike.direction of
                North ->
                    "0"

                East ->
                    "90"

                South ->
                    "180"

                West ->
                    "270"

        transformValue =
            String.join ""
                [ "translate("
                , toString (posX - bikeSize)
                , " "
                , toString (posY - bikeSize)
                , ") rotate("
                , rotation
                , " "
                , toString bikeSize
                , " "
                , toString bikeSize
                , ")"
                ]

        trailPoints =
            (bike.position :: bike.trail)
                |> List.map (\vec -> toString (getX vec) ++ "," ++ toString (getY vec))
                |> String.join " "
    in
        [ polyline [ fill "none", stroke bike.color, strokeWidth "3", points trailPoints ] []
        , g
            [ width (toString (2 * bikeSize))
            , height (toString (2 * bikeSize))
            , transform (transformValue)
            ]
            [ bikeForm bikeSize bike.color ]
        ]


bikeForm : Float -> String -> Svg msg
bikeForm bikeSize color =
    svg
        [ width (toString (2 * bikeSize))
        , height (toString (2 * bikeSize))
        , viewBox "0 0 210 297"
        ]
        [ g []
            [ Svg.path
                [ d "m 185.20834,126.15476 -66.86084,92.02605 -108.183103,-35.15082 0,-113.750451 L 118.3475,34.128718 Z"
                , Svg.Attributes.style ("fill:" ++ color)
                , transform "matrix(0,-1.690983,1.1396473,0,-39.07253,313.47282)"
                ]
                []
            ]
        ]
