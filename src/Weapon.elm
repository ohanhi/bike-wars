module Weapon exposing (..)

import BikePhysics
import Constants as C
import Math.Vector2 as Vec2 exposing (Vec2, getX, getY, vec2)
import Types exposing (..)


shoot : Weapon -> Direction -> Vec2 -> Trail -> Explosion
shoot weapon direction position trail =
    let
        ( x, y ) =
            Vec2.toTuple position

        arenaBorder =
            case direction of
                North ->
                    vec2 x 0

                South ->
                    vec2 x C.h

                West ->
                    vec2 0 y

                East ->
                    vec2 C.w y

        move =
            { position = position, nextPosition = arenaBorder }

        collisionPoint =
            BikePhysics.collision move trail []
                |> Maybe.withDefault arenaBorder
    in
    { center = collisionPoint, size = 50, ticksLeft = 10 }
