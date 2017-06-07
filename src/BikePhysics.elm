module BikePhysics exposing (collision, computeDirection, computePosition)

{-| Doctest imports

    >>> import Math.Vector2 exposing (vec2)
    >>> import Direction exposing (..)

-}

import Constants exposing (..)
import Direction exposing (..)
import Keyboard.Extra exposing (Key(..))
import Math.Vector2 as Vec2 exposing (Vec2, getX, getY, vec2)
import Types exposing (..)


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


computeDirection : Controls -> Direction -> Key -> Direction
computeDirection controls direction key =
    if key == controls.left then
        turnLeft direction
    else if key == controls.right then
        turnRight direction
    else
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
                    recurse (orderedTuple a b :: acc) (b :: tail)
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

        Horizontal _ ->
            Nothing


reduceHorizontal : Float -> Line -> Maybe Vec2
reduceHorizontal posX line =
    case line of
        Horizontal ( a, b ) ->
            if (getX a < posX) && (getX b > posX) then
                Just (vec2 posX (getY a))
            else
                Nothing

        Vertical _ ->
            Nothing


{-| This function should find any sort of collision with the current walls.

Vertical line, moving horizontally

    >>> collision 10.0 East (vec2 0 0) [[ vec2 1 -10, vec2 1 10 ]]
    Just (vec2 1 0)
    >>> collision 10.0 West (vec2 0 0) [[ vec2 1 -10, vec2 1 10 ]]
    Nothing

Horizontal line, moving vertically

    >>> collision 10.0 South (vec2 0 0) [[ vec2 -10 1, vec2 10 1 ]]
    Just (vec2 0 1)
    >>> collision 10.0 North (vec2 0 0) [[ vec2 -10 1, vec2 10 1 ]]
    Nothing

Horizontal line, moving horizontally

    >>> collision 10.0 East (vec2 0 0) [[ vec2 -10 1, vec2 10 1 ]]
    Just (vec2 0 1)
    >>> collision 10.0 West (vec2 0 0) [[ vec2 -10 1, vec2 10 1 ]]
    Just (vec2 0 1)

Vertical line, moving vertically

    >>> collision 10.0 South (vec2 0 0) [[ vec2 1 -10, vec2 1 10 ]]
    Just (vec2 1 0)
    >>> collision 10.0 North (vec2 0 0) [[ vec2 1 -10, vec2 1 10 ]]
    Just (vec2 1 0)

-}
collision : Float -> Direction -> Vec2 -> Trail -> Maybe Vec2
collision distance direction pos trail =
    let
        ( posX, posY ) =
            Vec2.toTuple pos

        lines =
            gameBounds ++ trailToLines trail

        verticals =
            List.filterMap (reduceVertical posY) lines

        horizontals =
            List.filterMap (reduceHorizontal posX) lines

        compareTo =
            List.filter (lessThanEpsilon << Vec2.distance pos)
                >> List.head

        lessThanEpsilon x =
            x <= distance

        maybeOr first second =
            if first /= Nothing then
                first
            else
                second
    in
    case direction of
        North ->
            maybeOr
                (compareTo (List.filter (\v -> getY v <= posY) horizontals))
                (compareTo verticals)

        South ->
            maybeOr
                (compareTo (List.filter (\v -> getY v >= posY) horizontals))
                (compareTo verticals)

        West ->
            maybeOr
                (compareTo (List.filter (\v -> getX v <= posX) verticals))
                (compareTo horizontals)

        East ->
            maybeOr
                (compareTo (List.filter (\v -> getX v >= posX) verticals))
                (compareTo horizontals)
