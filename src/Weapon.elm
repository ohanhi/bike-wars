module Weapon exposing (shoot, shootMegaBlaster)

import BikePhysics
import Constants as C
import Math.Vector2 as Vec2 exposing (Vec2, getX, getY, vec2)
import Types exposing (..)


shoot : { a | weapon : Weapon, direction : Direction, position : Vec2 } -> Trail -> ( Weapon, Maybe Explosion )
shoot { weapon, direction, position } trail =
    case weapon of
        MegaBlaster state ->
            shootMegaBlaster direction position trail state

        other ->
            ( other, Nothing )


shootMegaBlaster direction position trail state =
    let
        { x, y } =
            Vec2.toRecord position

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
    ( MegaBlaster { used = True }
    , if state.used then
        Nothing

      else
        Just { center = collisionPoint, size = 50, ticksLeft = 10 }
    )
