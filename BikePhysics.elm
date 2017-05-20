module BikePhysics exposing (..)

import Math.Vector2 as Vec2 exposing (Vec2, vec2, getX, getY)
import Keyboard.Extra exposing (Key(..))
import Constants exposing (..)
import Direction exposing (..)
import Types exposing (..)


cons : Vec2 -> Trail -> Trail
cons point trail =
    case trail of
        [] ->
            [ [ point ] ]

        first :: rest ->
            (point :: first) :: rest


computePosition : Direction -> Vec2 -> Float -> Vec2
computePosition direction position diff =
    let
        c =
            speedC * diff

        change =
            case direction of
                North ->
                    vec2 0 -c

                South ->
                    vec2 0 c

                East ->
                    vec2 c 0

                West ->
                    vec2 -c 0
    in
        Vec2.add position change


computeDirection : Direction -> Key -> Direction
computeDirection direction key =
    case key of
        ArrowLeft ->
            turnLeft direction

        ArrowRight ->
            turnRight direction

        _ ->
            direction


trailToLines : Trail -> List Line
trailToLines =
    let
        recurse acc list =
            case list of
                [] ->
                    acc

                _ :: [] ->
                    acc

                a :: b :: tail ->
                    recurse ((orderedTuple a b) :: acc) (b :: tail)
    in
        List.concatMap (recurse [])


orderedTuple : Vec2 -> Vec2 -> Line
orderedTuple a b =
    let
        (( aX, aY ) as tupleA) =
            Vec2.toTuple a

        (( bX, bY ) as tupleB) =
            Vec2.toTuple b
    in
        if aX == bX then
            -- vertical
            if aY < bY then
                Vertical ( a, b )
            else
                Vertical ( b, a )
        else if aX < bX then
            -- horizontal
            Horizontal ( a, b )
        else
            Horizontal ( b, a )


reduceVertical : Float -> Line -> Maybe Vec2
reduceVertical posY line =
    case line of
        Vertical ( a, b ) ->
            if (getY a < posY) && (getY b > posY) then
                Just (vec2 (getX a) posY)
            else
                Nothing

        _ ->
            Nothing


reduceHorizontal : Float -> Line -> Maybe Vec2
reduceHorizontal posX line =
    case line of
        Horizontal ( a, b ) ->
            if (getX a < posX) && (getX b > posX) then
                Just (vec2 posX (getY a))
            else
                Nothing

        _ ->
            Nothing


isCollision : Direction -> Vec2 -> Trail -> Bool
isCollision direction pos trail =
    let
        ( posX, posY ) =
            Vec2.toTuple pos

        lines =
            gameBounds ++ (trailToLines trail)

        verticals =
            List.filterMap (reduceVertical posY) lines

        horizontals =
            List.filterMap (reduceHorizontal posX) lines

        compareTo =
            (List.map (Vec2.distance pos)) >> (List.any lessThanEpsilon)

        lessThanEpsilon x =
            x < 5
    in
        case direction of
            North ->
                horizontals
                    |> List.filter (\v -> getY v < posY)
                    |> compareTo

            South ->
                horizontals
                    |> List.filter (\v -> getY v >= posY)
                    |> compareTo

            East ->
                verticals
                    |> List.filter (\v -> getX v > posX)
                    |> compareTo

            West ->
                verticals
                    |> List.filter (\v -> getX v <= posX)
                    |> compareTo
