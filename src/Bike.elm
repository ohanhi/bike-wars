module Bike exposing (..)

import BikePhysics exposing (..)
import Constants exposing (bikeSize, speedC)
import Direction exposing (..)
import Explosion
import Keyboard.Extra
import Math.Vector2 as Vec2 exposing (Vec2, getX, getY, vec2)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Types exposing (..)


initBike : Controls -> String -> ( Float, Float ) -> Direction -> Bike
initBike controls color ( x, y ) direction =
    { position = vec2 x y
    , trail = [ [ vec2 x y ] ]
    , collided = False
    , direction = direction
    , color = color
    , controls = controls
    }


frontWall : Bike -> List Vec2
frontWall bike =
    let
        vertical =
            [ vec2 (getX bike.position) (getY bike.position - bikeSize)
            , vec2 (getX bike.position) (getY bike.position + bikeSize)
            ]

        horizontal =
            [ vec2 (getX bike.position - bikeSize) (getY bike.position)
            , vec2 (getX bike.position + bikeSize) (getY bike.position)
            ]
    in
    case bike.direction of
        North ->
            horizontal

        East ->
            vertical

        South ->
            horizontal

        West ->
            vertical


move : Float -> Trail -> Bike -> ( Bike, Maybe Explosion )
move diff trail bike =
    let
        distance =
            speedC * diff

        nextPosition =
            computePosition bike.direction bike.position diff

        collisionPoint =
            collision { position = bike.position, nextPosition = nextPosition } trail

        ( collided, position, explosionPoint ) =
            case ( bike.collided, collisionPoint ) of
                ( False, Nothing ) ->
                    ( False, nextPosition, Nothing )

                ( False, Just point ) ->
                    ( True, point, Just point )

                ( True, _ ) ->
                    ( True, bike.position, Nothing )
    in
    ( { bike
        | position = position
        , collided = collided
      }
    , Maybe.map Explosion.forBike explosionPoint
    )


turn : Keyboard.Extra.Key -> Bike -> Bike
turn key bike =
    if bike.controls.left == key || bike.controls.right == key then
        { bike
            | direction = computeDirection bike.controls bike.direction key
            , trail = cons bike.position bike.trail
        }
    else
        bike


view : Bike -> List (Svg msg)
view bike =
    let
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
            cons bike.position bike.trail
                |> List.concatMap (List.map (\vec -> toString (getX vec) ++ "," ++ toString (getY vec)))
                |> String.join " "
    in
    [ polyline [ fill "none", stroke bike.color, strokeWidth (toString (bikeSize / 2)), points trailPoints ] []
    , g [ transform transformValue ] [ bikeForm bikeSize bike.color ]
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


cons : Vec2 -> Trail -> Trail
cons point trail =
    case trail of
        [] ->
            [ [ point ] ]

        first :: rest ->
            (point :: first) :: rest


omitLastSections : Trail -> Trail
omitLastSections trail =
    case trail of
        first :: rest ->
            List.drop 1 first :: rest

        other ->
            other
