module HelpersSpec exposing (spec)

import Expect
import Fuzz exposing (..)
import Helpers exposing (..)
import Math.Vector2 exposing (vec2)
import Test exposing (..)


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
        ]
