module BikePhysicsSpec exposing (spec)

import BikePhysics exposing (..)
import Expect
import Fuzz exposing (..)
import Math.Vector2 exposing (vec2)
import Test exposing (..)


coord : Fuzzer Float
coord =
    floatRange 2000 2000


spec : Test
spec =
    describe "BikePhysics"
        [ describe "collision"
            [ fuzz (tuple4 ( coord, coord, coord, coord )) "Vertical line, moving horizontally" <|
                \( lineX, startX, endX, y ) ->
                    Expect.equal
                        (collision { position = vec2 startX y, nextPosition = vec2 endX y }
                            [ [ vec2 lineX y, vec2 lineX (y + 10) ] ]
                        )
                    <|
                        if startX == endX then
                            Nothing
                        else if (startX <= lineX && lineX <= endX) || (lineX <= startX && endX <= lineX) then
                            Just (vec2 lineX y)
                        else
                            Nothing
            , fuzz (tuple4 ( coord, coord, coord, coord )) "Horizontal line, moving vertically" <|
                \( lineY, startY, endY, x ) ->
                    Expect.equal
                        (collision { position = vec2 x startY, nextPosition = vec2 x endY }
                            [ [ vec2 x lineY, vec2 (x + 10) lineY ] ]
                        )
                    <|
                        if startY == endY then
                            Nothing
                        else if (startY <= lineY && lineY <= endY) || (lineY <= startY && endY <= lineY) then
                            Just (vec2 x lineY)
                        else
                            Nothing
            ]
        ]
