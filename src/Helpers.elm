module Helpers exposing (..)

{-| Doctest imports

    import Math.Vector2 exposing (vec2)
    import Types exposing (..)

-}

import Math.Vector2 as Vec2 exposing (Vec2, getX, getY, vec2)
import Types exposing (..)


{-| Sort points on the 2D plane.

    sortPoints (vec2 0 0, vec2 10 0)
    --> (vec2 0 0, vec2 10 0)

    sortPoints (vec2 0 0, vec2 0 10)
    --> (vec2 0 0, vec2 0 10)

    sortPoints (vec2 10 0, vec2 0 0)
    --> (vec2 0 0, vec2 10 0)

    sortPoints (vec2 0 10, vec2 0 0)
    --> (vec2 0 0, vec2 0 10)

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


{-|

    [[ vec2 100 100, vec2 90 100, vec2 90 120 ]]
        |> trailToLines
        |> linesToTrail
    --> [[vec2 100 100, vec2 90 100, vec2 90 120]]

    [ [ vec2 100 100, vec2 90 100, vec2 90 120, vec2 80 120 ]
    , [ vec2 0 0, vec2 10 0, vec2 10 10 ]
    ]
        |> trailToLines
        |> linesToTrail
    --> [ [ vec2 100 100, vec2 90 100, vec2 90 120, vec2 80 120 ]
    --> , [ vec2 0 0, vec2 10 0, vec2 10 10 ]
    --> ]

-}
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


{-|

    trailToLines [[vec2 0 0, vec2 10 0, vec2 10 10]]
    --> [ Horizontal (vec2 0 0, vec2 10 0), Vertical (vec2 10 0, vec2 10 10) ]

    trailToLines [[vec2 0 0, vec2 10 0], [vec2 100 10, vec2 0 10]]
    --> [ Horizontal (vec2 0 0, vec2 10 0), Horizontal (vec2 100 10, vec2 0 10) ]

    trailToLines
        [ [ vec2 324 100
          , vec2 324 124
          , vec2 278 124
          , vec2 278 150
          , vec2 309 150
          ]
        , [ vec2 339 150
          , vec2 380 150
          ]
        ]
    --> [ Vertical   (vec2 324 100, vec2 324 124)
    --> , Horizontal (vec2 324 124, vec2 278 124)
    --> , Vertical   (vec2 278 124, vec2 278 150)
    --> , Horizontal (vec2 278 150, vec2 309 150)
    --> , Horizontal (vec2 339 150, vec2 380 150)
    --> ]

-}
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
toLine a b =
    let
        ( aX, aY ) =
            Vec2.toTuple a

        ( bX, bY ) =
            Vec2.toTuple b
    in
    if a == b then
        Nothing
    else if aX == bX then
        Just (Vertical ( a, b ))
    else if aY == bY then
        Just (Horizontal ( a, b ))
    else
        Nothing
            |> Debug.log ("Line invalid: " ++ toString ( a, b ))


tryCollision : (( Vec2, Vec2 ) -> Maybe Vec2) -> List ( Vec2, Vec2 ) -> Maybe Vec2
tryCollision method linePoints =
    linePoints
        |> List.filterMap method
        |> List.head


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
