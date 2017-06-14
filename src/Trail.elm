module Trail exposing (..)

{-| Doctest imports

    >>> import Math.Vector2 exposing (vec2)
    >>> import Types exposing (..)

-}

import Helpers exposing (..)
import Math.Vector2 as Vec2 exposing (Vec2, getX, getY, setX, setY, vec2)
import Types exposing (..)


{-| Remove parts of the trail that are under the explosion.

Simple horizontals

    >>> breakLines
    ...   { center = vec2 0 0, size = 100, ticksLeft = 30 }
    ...   [ Horizontal (vec2 0 0, vec2 100 0) ]
    [ Horizontal (vec2 50 0, vec2 100 0) ]

    >>> breakLines
    ...   { center = vec2 100 0, size = 100, ticksLeft = 30 }
    ...   [ Horizontal (vec2 0 0, vec2 100 0) ]
    [ Horizontal (vec2 0 0, vec2 50 0) ]

    >>> breakLines
    ...   { center = vec2 50 0, size = 50, ticksLeft = 30 }
    ...   [ Horizontal (vec2 0 0, vec2 100 0) ]
    [ Horizontal (vec2 0 0, vec2 25 0), Horizontal (vec2 75 0, vec2 100 0) ]

    >>> breakLines
    ...   { center = vec2 0 0, size = 50, ticksLeft = 30 }
    ...   [ Horizontal (vec2 -10 0, vec2 10 0) ]
    []

Simple verticals

    >>> breakLines
    ...   { center = vec2 0 0, size = 100, ticksLeft = 30 }
    ...   [ Vertical (vec2 0 0, vec2 0 100) ]
    [ Vertical (vec2 0 50, vec2 0 100) ]

    >>> breakLines
    ...   { center = vec2 0 100, size = 100, ticksLeft = 30 }
    ...   [ Vertical (vec2 0 0, vec2 0 100) ]
    [ Vertical (vec2 0 0, vec2 0 50) ]

    >>> breakLines
    ...   { center = vec2 0 50, size = 50, ticksLeft = 30 }
    ...   [ Vertical (vec2 0 0, vec2 0 100) ]
    [ Vertical (vec2 0 0, vec2 0 25), Vertical (vec2 0 75, vec2 0 100) ]

    >>> breakLines
    ...   { center = vec2 0 0, size = 50, ticksLeft = 30 }
    ...   [ Vertical (vec2 0 -10, vec2 0 10) ]
    []

More complex

    >>> breakLines
    ...   { center = vec2 0 0, size = 100, ticksLeft = 30 }
    ...   [ Vertical (vec2 -10 -300, vec2 -10 100)
    ...   , Horizontal (vec2 -10 100, vec2 200 100)
    ...   ]
    [ Vertical (vec2 -10 -300, vec2 -10 -50)
    , Vertical (vec2 -10 50, vec2 -10 100)
    , Horizontal (vec2 -10 100, vec2 200 100)
    ]

    >>> breakLines
    ...   { center = vec2 0 0, size = 100, ticksLeft = 30 }
    ...   [ Vertical (vec2 -10 -300, vec2 -10 0)
    ...   , Horizontal (vec2 -10 0, vec2 200 0)
    ...   ]
    [ Vertical (vec2 -10 -300, vec2 -10 -50)
    , Horizontal (vec2 50 0, vec2 200 0)
    ]

    >>> breakLines
    ...   { center = vec2 0 0, size = 100, ticksLeft = 30 }
    ...   [ Vertical (vec2 -10 -300, vec2 -10 0)
    ...   , Horizontal (vec2 -10 0, vec2 0 0)
    ...   , Vertical (vec2 0 0, vec2 0 10)
    ...   , Horizontal (vec2 -100 10, vec2 0 10)
    ...   ]
    [ Vertical (vec2 -10 -300, vec2 -10 -50)
    , Horizontal (vec2 -100 10, vec2 -50 10)
    ]

-}
breakLines : Explosion -> List Line -> List Line
breakLines explosion lines =
    List.concatMap (cutWith explosion) lines


cutWith : Explosion -> Line -> List Line
cutWith explosion line =
    let
        ( cx, cy ) =
            Vec2.toTuple explosion.center

        r =
            explosion.size / 2

        bounds =
            { w = cx - r
            , e = cx + r
            , n = cy - r
            , s = cy + r
            }
    in
    case line of
        Horizontal ( a, b ) ->
            horizontalPairs bounds ( a, b )
                |> List.map Horizontal

        Vertical ( a, b ) ->
            verticalPairs bounds ( a, b )
                |> List.map Vertical


horizontalPairs :
    { a | e : Float, n : Float, s : Float, w : Float }
    -> ( Vec2, Vec2 )
    -> List ( Vec2, Vec2 )
horizontalPairs { n, w, s, e } ( a, b ) =
    if isBetween n s (getY a) then
        let
            aIsBetween =
                isBetween w e (getX a)

            bIsBetween =
                isBetween w e (getX b)
        in
        case ( aIsBetween, bIsBetween ) of
            ( True, True ) ->
                []

            ( True, False ) ->
                [ ( setX e a, b ) ]

            ( False, True ) ->
                [ ( a, setX w b ) ]

            ( False, False ) ->
                [ ( a, vec2 w (getY a) )
                , ( vec2 e (getY b), b )
                ]
    else
        [ ( a, b ) ]


verticalPairs :
    { a | e : Float, n : Float, s : Float, w : Float }
    -> ( Vec2, Vec2 )
    -> List ( Vec2, Vec2 )
verticalPairs { n, w, s, e } ( a, b ) =
    if isBetween w e (getX a) then
        let
            aIsBetween =
                isBetween n s (getY a)

            bIsBetween =
                isBetween n s (getY b)
        in
        case ( aIsBetween, bIsBetween ) of
            ( True, True ) ->
                []

            ( True, False ) ->
                [ ( setY s a, b ) ]

            ( False, True ) ->
                [ ( a, setY n b ) ]

            ( False, False ) ->
                [ ( a, vec2 (getX a) n )
                , ( vec2 (getX b) s, b )
                ]
    else
        [ ( a, b ) ]
