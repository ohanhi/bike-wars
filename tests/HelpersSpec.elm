module HelpersSpec exposing (spec)

import Expect
import Fuzz exposing (..)
import Helpers exposing (..)
import Math.Vector2 exposing (vec2)
import Test exposing (..)
import Types exposing (..)


coord : Fuzzer Float
coord =
    intRange -1000 2000
        |> Fuzz.map toFloat


size : Fuzzer Float
size =
    intRange 1 100
        |> Fuzz.map toFloat


spec : Test
spec =
    describe "Helpers"
        [ describe "tryObstacleCollision"
            [ fuzz (tuple4 ( coord, coord, coord, size )) "Moving horizontally, startX inside" <|
                \( startX, endX, y, size ) ->
                    tryObstacleCollision
                        ( vec2 startX y, vec2 endX y )
                        { n = y - size, s = y + size, e = startX + size, w = startX - size }
                        |> Expect.equal (Just (vec2 startX y))
            , fuzz (tuple4 ( coord, coord, coord, size )) "Moving horizontally, endX inside" <|
                \( startX, endX, y, size ) ->
                    tryObstacleCollision
                        ( vec2 startX y, vec2 endX y )
                        { n = y - size, s = y + size, e = endX + size, w = endX - size }
                        |> Expect.notEqual Nothing
            , fuzz (tuple4 ( coord, coord, coord, size )) "Moving vertically, startY inside" <|
                \( startY, endY, x, size ) ->
                    tryObstacleCollision
                        ( vec2 x startY, vec2 x endY )
                        { n = startY - size, s = startY + size, e = x + size, w = x - size }
                        |> Expect.equal (Just (vec2 x startY))
            , fuzz (tuple4 ( coord, coord, coord, size )) "Moving vertically, endY inside" <|
                \( startY, endY, x, size ) ->
                    tryObstacleCollision
                        ( vec2 x startY, vec2 x endY )
                        { n = endY - size, s = endY + size, e = x + size, w = x - size }
                        |> Expect.notEqual Nothing
            ]
        , Test.describe "sortPoints" <|
            [ Test.test "Example: " <|
                \() ->
                    sortPoints ( vec2 0 0, vec2 10 0 )
                        |> Expect.equal ( vec2 0 0, vec2 10 0 )
            , Test.test "Example: 2" <|
                \() ->
                    sortPoints ( vec2 0 0, vec2 0 10 )
                        |> Expect.equal ( vec2 0 0, vec2 0 10 )
            , Test.test "Example: 3" <|
                \() ->
                    sortPoints ( vec2 10 0, vec2 0 0 )
                        |> Expect.equal ( vec2 0 0, vec2 10 0 )
            , Test.test "Example: 4" <|
                \() ->
                    sortPoints ( vec2 0 10, vec2 0 0 )
                        |> Expect.equal ( vec2 0 0, vec2 0 10 )
            ]
        , Test.describe "trailToLines |> linesToTrail" <|
            [ Test.test "Example: 1" <|
                \() ->
                    Expect.equal
                        ([ [ vec2 100 100, vec2 90 100, vec2 90 120 ] ]
                            |> trailToLines
                            |> linesToTrail
                        )
                        [ [ vec2 100 100, vec2 90 100, vec2 90 120 ] ]
            , Test.test "Example: 2" <|
                \() ->
                    Expect.equal
                        ([ [ vec2 100 100, vec2 90 100, vec2 90 120, vec2 80 120 ]
                         , [ vec2 0 0, vec2 10 0, vec2 10 10 ]
                         ]
                            |> trailToLines
                            |> linesToTrail
                        )
                        [ [ vec2 100 100, vec2 90 100, vec2 90 120, vec2 80 120 ]
                        , [ vec2 0 0, vec2 10 0, vec2 10 10 ]
                        ]
            ]
        , Test.describe "trailToLines" <|
            [ Test.test "Example: 1" <|
                \() ->
                    Expect.equal
                        (trailToLines [ [ vec2 0 0, vec2 10 0, vec2 10 10 ] ])
                        [ Horizontal ( vec2 0 0, vec2 10 0 ), Vertical ( vec2 10 0, vec2 10 10 ) ]
            , Test.test "Example: 2" <|
                \() ->
                    Expect.equal
                        (trailToLines [ [ vec2 0 0, vec2 10 0 ], [ vec2 100 10, vec2 0 10 ] ])
                        [ Horizontal ( vec2 0 0, vec2 10 0 ), Horizontal ( vec2 100 10, vec2 0 10 ) ]
            , Test.test "Example: 3" <|
                \() ->
                    Expect.equal
                        (trailToLines
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
                        )
                        [ Vertical ( vec2 324 100, vec2 324 124 )
                        , Horizontal ( vec2 324 124, vec2 278 124 )
                        , Vertical ( vec2 278 124, vec2 278 150 )
                        , Horizontal ( vec2 278 150, vec2 309 150 )
                        , Horizontal ( vec2 339 150, vec2 380 150 )
                        ]
            ]
        ]
