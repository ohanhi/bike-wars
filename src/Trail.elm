module Trail exposing (breakIfNecessary, breakLines, breakTrail, cutWith, horizontalPairs, verticalPairs)

import Explosion
import Helpers exposing (..)
import Math.Vector2 as Vec2 exposing (Vec2, getX, getY, setX, setY, vec2)
import Types exposing (..)


breakIfNecessary : List Explosion -> Trail -> Trail
breakIfNecessary explosions trail =
    case explosions of
        [] ->
            trail

        a :: rest ->
            trail
                |> breakTrail a
                |> breakIfNecessary rest


breakTrail : Explosion -> Trail -> Trail
breakTrail explosion trail =
    trail
        |> trailToLines
        |> breakLines explosion
        |> linesToTrail


{-| Remove parts of the trail that are under the explosion.

    import Math.Vector2 exposing (vec2)
    import Types exposing (..)
    import Helpers exposing (..)

Simple horizontals

    breakLines
      { center = vec2 0 0, size = 100, ticksLeft = 30 }
      [ Horizontal (vec2 100 0, vec2 100 0) ]
    --> [ Horizontal (vec2 100 0, vec2 100 0) ]

    breakLines
      { center = vec2 0 0, size = 100, ticksLeft = 30 }
      [ Horizontal (vec2 0 0, vec2 100 0) ]
    --> [ Horizontal (vec2 50 0, vec2 100 0) ]

    breakLines
      { center = vec2 100 0, size = 100, ticksLeft = 30 }
      [ Horizontal (vec2 100 0, vec2 0 0) ]
    --> [ Horizontal (vec2 50 0, vec2 0 0) ]

    breakLines
      { center = vec2 50 0, size = 50, ticksLeft = 30 }
      [ Horizontal (vec2 0 0, vec2 100 0) ]
    --> [ Horizontal (vec2 0 0, vec2 25 0), Horizontal (vec2 75 0, vec2 100 0) ]

    breakLines
      { center = vec2 210 120, size = 30, ticksLeft = 1 }
      [ Horizontal ( vec2 246 120, vec2 20 120) ]
    --> [ Horizontal (vec2 246 120, vec2 225 120)
    --> , Horizontal (vec2 195 120, vec2 20 120)
    --> ]

    breakLines
      { center = vec2 0 0, size = 50, ticksLeft = 30 }
      [ Horizontal (vec2 0 0, vec2 20 0) ]
    --> []

Simple verticals

    breakLines
      { center = vec2 0 0, size = 100, ticksLeft = 30 }
      [ Vertical (vec2 0 0, vec2 0 100) ]
    --> [ Vertical (vec2 0 50, vec2 0 100) ]

    breakLines
      { center = vec2 0 100, size = 100, ticksLeft = 30 }
      [ Vertical (vec2 0 0, vec2 0 100), Vertical (vec2 0 200, vec2 0 100) ]
    --> [ Vertical (vec2 0 0, vec2 0 50), Vertical (vec2 0 200, vec2 0 150) ]

    breakLines
      { center = vec2 0 50, size = 50, ticksLeft = 30 }
      [ Vertical (vec2 0 0, vec2 0 100) ]
    --> [ Vertical (vec2 0 0, vec2 0 25), Vertical (vec2 0 75, vec2 0 100) ]

    breakLines
      { center = vec2 0 0, size = 50, ticksLeft = 30 }
      [ Vertical (vec2 0 0, vec2 0 20) ]
    --> []

More complex

    breakLines
      { center = vec2 1000 1000, size = 100, ticksLeft = 30 }
      [ Vertical (vec2 990 700, vec2 990 1100)
      , Horizontal (vec2 990 1100, vec2 200 1100)
      ]
    --> [ Vertical (vec2 990 700, vec2 990 950)
    --> , Vertical (vec2 990 1050, vec2 990 1100)
    --> , Horizontal (vec2 990 1100, vec2 200 1100)
    --> ]

    breakLines
      { center = vec2 0 0, size = 100, ticksLeft = 30 }
      [ Vertical (vec2 -10 -300, vec2 -10 0)
      , Horizontal (vec2 -10 0, vec2 200 0)
      ]
    --> [ Vertical (vec2 -10 -300, vec2 -10 -50)
    --> , Horizontal (vec2 50 0, vec2 200 0)
    --> ]

    breakLines
      { center = vec2 0 0, size = 100, ticksLeft = 30 }
      [ Vertical (vec2 -10 -300, vec2 -10 0)
      , Horizontal (vec2 -10 0, vec2 0 0)
      , Vertical (vec2 0 0, vec2 0 10)
      , Horizontal (vec2 -100 10, vec2 0 10)
      ]
    --> [ Vertical (vec2 -10 -300, vec2 -10 -50)
    --> , Horizontal (vec2 -100 10, vec2 -50 10)
    --> ]

    breakLines
      { center = vec2 365 150, size = 30, ticksLeft = 3 }
      (trailToLines [[ vec2 185 150, vec2 20 150 ]])
    --> (trailToLines [[ vec2 185 150, vec2 20 150 ]])

    breakLines
        { center = vec2 324 150, size = 30, ticksLeft = 30 }
        (trailToLines
            [ [ vec2 324 149
              , vec2 324 124
              , vec2 278 124
              , vec2 278 150
              , vec2 380 150
              ]
            ]
        )
    --> trailToLines
    -->     [ [ vec2 324 135
    -->       , vec2 324 124
    -->       , vec2 278 124
    -->       , vec2 278 150
    -->       , vec2 309 150
    -->       ]
    -->     , [ vec2 339 150
    -->       , vec2 380 150
    -->       ]
    -->     ]

-}
breakLines : Explosion -> List Line -> List Line
breakLines explosion lines =
    List.concatMap (cutWith explosion) lines


cutWith : Explosion -> Line -> List Line
cutWith explosion line =
    let
        bounds =
            Explosion.toObstacle explosion
    in
    case line of
        Horizontal points ->
            horizontalPairs bounds points
                |> List.map Horizontal

        Vertical points ->
            verticalPairs bounds points
                |> List.map Vertical


horizontalPairs :
    { a | e : Float, n : Float, s : Float, w : Float }
    -> ( Vec2, Vec2 )
    -> List ( Vec2, Vec2 )
horizontalPairs { n, w, s, e } (( end, start ) as points) =
    let
        ( small, large ) =
            sortPoints points

        smallIsStart =
            small == start

        mapSmall =
            if smallIsStart then
                Tuple.mapSecond

            else
                Tuple.mapFirst

        mapLarge =
            if smallIsStart then
                Tuple.mapFirst

            else
                Tuple.mapSecond
    in
    if isBetween ( n, s ) (getY small) then
        let
            smallIsBetween =
                isBetween ( w, e ) (getX small)

            largeIsBetween =
                isBetween ( w, e ) (getX large)

            explosionIsBetween =
                getX small < w && e < getX large

            y =
                getY small
        in
        case ( smallIsBetween, largeIsBetween, explosionIsBetween ) of
            ( False, False, False ) ->
                [ points ]

            ( False, False, True ) ->
                if smallIsStart then
                    [ mapSmall (setX e) points
                    , mapLarge (setX w) points
                    ]

                else
                    [ mapLarge (setX w) points
                    , mapSmall (setX e) points
                    ]

            ( True, False, _ ) ->
                [ mapSmall (setX e) points ]

            ( False, True, _ ) ->
                [ mapLarge (setX w) points ]

            ( True, True, _ ) ->
                []

    else
        [ points ]


verticalPairs :
    { a | e : Float, n : Float, s : Float, w : Float }
    -> ( Vec2, Vec2 )
    -> List ( Vec2, Vec2 )
verticalPairs { n, w, s, e } (( end, start ) as points) =
    let
        ( small, large ) =
            sortPoints points

        smallIsStart =
            small == start

        mapSmall =
            if smallIsStart then
                Tuple.mapSecond

            else
                Tuple.mapFirst

        mapLarge =
            if smallIsStart then
                Tuple.mapFirst

            else
                Tuple.mapSecond
    in
    if isBetween ( w, e ) (getX small) then
        let
            smallIsBetween =
                isBetween ( n, s ) (getY small)

            largeIsBetween =
                isBetween ( n, s ) (getY large)

            explosionIsBetween =
                getY small < n && s < getY large
        in
        case ( smallIsBetween, largeIsBetween, explosionIsBetween ) of
            ( False, False, False ) ->
                [ points ]

            ( True, True, _ ) ->
                []

            ( True, False, _ ) ->
                [ mapSmall (setY s) points ]

            ( False, True, _ ) ->
                [ mapLarge (setY n) points ]

            ( False, False, _ ) ->
                if smallIsStart then
                    [ mapSmall (setY s) points
                    , mapLarge (setY n) points
                    ]

                else
                    [ mapLarge (setY n) points
                    , mapSmall (setY s) points
                    ]

    else
        [ points ]
