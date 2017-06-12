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


{-| This function should find any sort of collision with the current walls.

Vertical line, moving horizontally

    >>> collision
    ... { position = vec2 0 0, nextPosition = vec2 -10 0 }
    ... [[ vec2 1 -10, vec2 1 10 ]] -- West
    Nothing
    >>> collision
    ... { position = vec2 0 0, nextPosition = vec2 10 0 }
    ... [[ vec2 1 -10, vec2 1 10 ]] -- East
    Just (vec2 1 0)
    >>> collision
    ... { position = vec2 10 0, nextPosition = vec2 0 0 }
    ... [[ vec2 1 -10, vec2 1 10 ]] -- West
    Just (vec2 1 0)
    >>> collision
    ... { position = vec2 0 0, nextPosition = vec2 5 0 }
    ... [[ vec2 10 -10, vec2 10 10 ]] -- East
    Nothing

Horizontal line, moving vertically

    >>> collision
    ... { position = vec2 0 0, nextPosition = vec2 0 10 }
    ... [[ vec2 -10 1, vec2 10 1 ]]
    Just (vec2 0 1)
    >>> collision
    ... { position = vec2 0 0, nextPosition = vec2 0 -10 }
    ... [[ vec2 -10 1, vec2 10 1 ]]
    Nothing

Horizontal line, moving horizontally

    >>> collision
    ... { position = vec2 0 0, nextPosition = vec2 10 0 }
    ... [[ vec2 -10 1, vec2 10 1 ]]
    Nothing

Vertical line, moving vertically

    >>> collision
    ... { position = vec2 0 0, nextPosition = vec2 0 10 }
    ... [[ vec2 1 -10, vec2 1 10 ]] -- North
    Nothing

-}
collision : { position : Vec2, nextPosition : Vec2 } -> Trail -> Maybe Vec2
collision { position, nextPosition } trail =
    let
        lines =
            trailToLines trail

        moveLine =
            orderedTuple position nextPosition
    in
    case moveLine of
        Horizontal points ->
            lines
                |> List.filterMap onlyVertical
                |> tryCollision (horizontalOrthogonal points)

        Vertical points ->
            lines
                |> List.filterMap onlyHorizontal
                |> tryCollision (verticalOrthogonal points)


tryCollision : (( Vec2, Vec2 ) -> Maybe Vec2) -> List ( Vec2, Vec2 ) -> Maybe Vec2
tryCollision method linePoints =
    linePoints
        |> List.filterMap method
        |> List.head


horizontalOrthogonal : ( Vec2, Vec2 ) -> ( Vec2, Vec2 ) -> Maybe Vec2
horizontalOrthogonal ( a, b ) ( trailA, trailB ) =
    if isBetweenWith getX a b trailA && isBetweenWith getY trailA trailB a then
        Just (vec2 (getX trailA) (getY a))
    else
        Nothing


verticalOrthogonal : ( Vec2, Vec2 ) -> ( Vec2, Vec2 ) -> Maybe Vec2
verticalOrthogonal ( a, b ) ( trailA, trailB ) =
    if isBetweenWith getY a b trailA && isBetweenWith getX trailA trailB a then
        Just (vec2 (getX a) (getY trailA))
    else
        Nothing


isBetweenWith : (Vec2 -> Float) -> Vec2 -> Vec2 -> Vec2 -> Bool
isBetweenWith getN small large trailEnd =
    getN small <= getN trailEnd && getN large >= getN trailEnd


onlyVertical : Line -> Maybe ( Vec2, Vec2 )
onlyVertical line =
    case line of
        Vertical points ->
            Just points

        Horizontal _ ->
            Nothing


onlyHorizontal : Line -> Maybe ( Vec2, Vec2 )
onlyHorizontal line =
    case line of
        Horizontal points ->
            Just points

        Vertical _ ->
            Nothing
