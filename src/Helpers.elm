module Helpers exposing (..)

import Math.Vector2 as Vec2 exposing (Vec2, getX, getY, vec2)
import Types exposing (..)


linesToTrail : List Line -> Trail
linesToTrail =
    List.map
        (\line ->
            case line of
                Horizontal ( a, b ) ->
                    [ a, b ]

                Vertical ( a, b ) ->
                    [ a, b ]
        )


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
                    recurse (toLine a b :: acc) (b :: tail)
    in
    List.concatMap (recurse [])


toLine : Vec2 -> Vec2 -> Line
toLine a b =
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


isBetween : Float -> Float -> Float -> Bool
isBetween small large point =
    small <= point && large >= point


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
