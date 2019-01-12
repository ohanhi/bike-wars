module Helpers exposing (horizontalOrthogonal, isBetween, isBetweenWith, linesToTrail, onlyHorizontal, onlyVertical, sortPoints, toLine, trailToLines, tryCollision, tryObstacleCollision, verticalOrthogonal)

import Math.Vector2 as Vec2 exposing (Vec2, getX, getY, vec2)
import Types exposing (..)


{-| Sort points on the 2D plane.
-}
sortPoints : ( Vec2, Vec2 ) -> ( Vec2, Vec2 )
sortPoints ( a, b ) =
    if
        (getX a == getX b && getY a < getY b)
            || (getY a == getY b && getX a < getX b)
    then
        ( a, b )

    else
        ( b, a )


linesToTrail : List Line -> Trail
linesToTrail =
    List.foldr
        (\line acc ->
            let
                ( start, end ) =
                    case line of
                        Horizontal points ->
                            points

                        Vertical points ->
                            points
            in
            case acc of
                [] ->
                    [ [ start, end ] ]

                accHead :: accTail ->
                    if List.head accHead == Just end then
                        (start :: accHead) :: accTail

                    else
                        [ start, end ] :: acc
        )
        []


trailToLines : Trail -> List Line
trailToLines trail =
    let
        recurse : List (Maybe Line) -> List Vec2 -> List (Maybe Line)
        recurse acc list =
            case list of
                [] ->
                    acc

                _ :: [] ->
                    acc

                a :: b :: tail ->
                    recurse (toLine a b :: acc) (b :: tail)
    in
    trail
        |> List.map (List.reverse << recurse [])
        |> List.concat
        |> List.filterMap identity


toLine : Vec2 -> Vec2 -> Maybe Line
toLine vecA vecB =
    let
        a =
            Vec2.toRecord vecA

        b =
            Vec2.toRecord vecB
    in
    if a == b then
        Nothing

    else if a.x == b.x then
        Just (Vertical ( vecA, vecB ))

    else if a.y == b.y then
        Just (Horizontal ( vecA, vecB ))

    else
        Debug.todo "Line invalid" ( a, b )


tryCollision : (( Vec2, Vec2 ) -> Maybe Vec2) -> List ( Vec2, Vec2 ) -> Maybe Vec2
tryCollision method linePoints =
    linePoints
        |> List.filterMap method
        |> List.head


tryObstacleCollision : ( Vec2, Vec2 ) -> Obstacle -> Maybe Vec2
tryObstacleCollision ( a, b ) { n, e, s, w } =
    let
        diff point =
            isBetween ( w, e ) (getX point) && isBetween ( n, s ) (getY point)
    in
    if diff a then
        Just a

    else if diff b then
        Just b

    else
        Nothing


horizontalOrthogonal : ( Vec2, Vec2 ) -> ( Vec2, Vec2 ) -> Maybe Vec2
horizontalOrthogonal move trail =
    let
        ( a, b ) =
            sortPoints move

        ( trailA, trailB ) =
            sortPoints trail
    in
    if isBetweenWith getX ( a, b ) trailA && isBetweenWith getY ( trailA, trailB ) a then
        Just (vec2 (getX trailA) (getY a))

    else
        Nothing


verticalOrthogonal : ( Vec2, Vec2 ) -> ( Vec2, Vec2 ) -> Maybe Vec2
verticalOrthogonal move trail =
    let
        ( a, b ) =
            sortPoints move

        ( trailA, trailB ) =
            sortPoints trail
    in
    if isBetweenWith getY ( a, b ) trailA && isBetweenWith getX ( trailA, trailB ) a then
        Just (vec2 (getX a) (getY trailA))

    else
        Nothing


isBetween : ( Float, Float ) -> Float -> Bool
isBetween ( small, large ) point =
    small <= point && large >= point


isBetweenWith : (Vec2 -> Float) -> ( Vec2, Vec2 ) -> Vec2 -> Bool
isBetweenWith getN ( small, large ) point =
    getN small <= getN point && getN large >= getN point


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
