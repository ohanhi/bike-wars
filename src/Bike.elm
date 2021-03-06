module Bike exposing (bikeForm, cons, frontWall, initBike, omitLastSections, shoot, swapPosition, toObstacle, turn, update, view)

import BikePhysics exposing (..)
import Constants exposing (bikeSize, speedC)
import Explosion
import Keyboard exposing (Key)
import Math.Vector2 as Vec2 exposing (Vec2, getX, getY, vec2)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Trail
import Types exposing (..)
import Weapon


initBike : Controls -> String -> Vec2 -> Direction -> Weapon -> Bike
initBike controls color pos direction weapon =
    { position = pos
    , trail = [ [ pos, pos ] ]
    , collided = False
    , direction = direction
    , color = color
    , controls = controls
    , weapon = weapon
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


update : Float -> List Explosion -> { current : Bike, other : Bike } -> ( Bike, Maybe Explosion )
update diff explosions { current, other } =
    let
        walls =
            frontWall other
                :: omitLastSections current.trail
                ++ other.trail
                ++ Constants.gameBounds

        nextPosition =
            computePosition current.direction current.position diff

        collisionPoint =
            collision
                { position = current.position, nextPosition = nextPosition }
                walls
                (toObstacle other :: List.map Explosion.toObstacle explosions)

        ( collided, position, explosionPoint ) =
            case ( current.collided, collisionPoint ) of
                ( False, Just point ) ->
                    -- fresh collision
                    ( True, point, Just point )

                ( True, _ ) ->
                    -- old collision
                    ( True, current.position, Nothing )

                ( False, Nothing ) ->
                    -- all good
                    ( False, nextPosition, Nothing )

        trail =
            if collided then
                current.trail
                    |> Trail.breakIfNecessary explosions

            else
                current.trail
                    |> swapPosition current.position
                    |> Trail.breakIfNecessary explosions
    in
    ( { current
        | position = position
        , collided = collided
        , trail = trail
      }
    , Maybe.map Explosion.forBike explosionPoint
    )


shoot : Key -> { current : Bike, other : Bike } -> ( Bike, Maybe Explosion )
shoot key { current, other } =
    if key == current.controls.up then
        let
            ( weapon, explosion ) =
                Weapon.shoot current (current.trail ++ other.trail)
        in
        ( { current | weapon = weapon }, explosion )

    else
        ( current, Nothing )


turn : Key -> Bike -> Bike
turn key bike =
    if bike.controls.left == key || bike.controls.right == key then
        { bike
            | direction = computeDirection bike.controls bike.direction key
            , trail =
                bike.trail
                    |> swapPosition bike.position
                    |> cons bike.position
        }

    else
        bike


toObstacle : Bike -> Obstacle
toObstacle bike =
    let
        { x, y } =
            Vec2.toRecord bike.position

        r =
            bikeSize / 2
    in
    { w = x - r
    , e = x + r
    , n = y - r
    , s = y + r
    }


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
                , String.fromFloat (posX - bikeSize)
                , " "
                , String.fromFloat (posY - bikeSize)
                , ") rotate("
                , rotation
                , " "
                , String.fromFloat bikeSize
                , " "
                , String.fromFloat bikeSize
                , ")"
                ]

        trail =
            bike.trail
                |> List.map
                    (List.map (\vec -> String.fromFloat (getX vec) ++ "," ++ String.fromFloat (getY vec))
                        >> String.join " "
                        >> (\polylinePoints ->
                                polyline
                                    [ fill "none"
                                    , stroke bike.color
                                    , strokeWidth (String.fromFloat (bikeSize / 2))
                                    , strokeLinecap "square"
                                    , points polylinePoints
                                    ]
                                    []
                           )
                    )

        bikeView =
            if bike.collided then
                []

            else
                [ g [ transform transformValue ] [ bikeForm bikeSize bike.color ] ]
    in
    trail ++ bikeView


bikeForm : Float -> String -> Svg msg
bikeForm bikeSize color =
    svg
        [ width (String.fromFloat (2 * bikeSize))
        , height (String.fromFloat (2 * bikeSize))
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


swapPosition : Vec2 -> Trail -> Trail
swapPosition point trail =
    case trail of
        (head :: tail) :: rest ->
            (point :: tail) :: rest

        _ ->
            trail


omitLastSections : Trail -> Trail
omitLastSections trail =
    case trail of
        first :: rest ->
            List.drop 2 first :: rest

        other ->
            other
