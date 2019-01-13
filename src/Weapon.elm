module Weapon exposing (initAfterBurner, initAssaultBazooka, initMegaBlaster, shoot, shootAssaultBazooka, shootMegaBlaster)

import BikePhysics
import Constants as C
import Math.Vector2 as Vec2 exposing (Vec2, getX, getY, vec2)
import Types exposing (..)


initMegaBlaster : Weapon
initMegaBlaster =
    MegaBlaster { used = False }


initAssaultBazooka : Weapon
initAssaultBazooka =
    AssaultBazooka { shotsLeft = 3 }


initAfterBurner : Weapon
initAfterBurner =
    Afterburner { ticksLeft = 60 }


shoot : { a | weapon : Weapon, direction : Direction, position : Vec2 } -> Trail -> ( Weapon, Maybe Explosion )
shoot { weapon, direction, position } trail =
    case weapon of
        MegaBlaster state ->
            shootMegaBlaster direction position trail state

        AssaultBazooka state ->
            shootAssaultBazooka direction position trail state

        other ->
            ( other, Nothing )


shootMegaBlaster : Direction -> Vec2 -> Trail -> { used : Bool } -> ( Weapon, Maybe Explosion )
shootMegaBlaster direction position trail state =
    ( MegaBlaster { used = True }
    , if state.used then
        Nothing

      else
        Just
            (projectExplosionForwards
                direction
                position
                trail
                { center = vec2 0 0, size = 50, ticksLeft = 10 }
            )
    )


shootAssaultBazooka : Direction -> Vec2 -> Trail -> { shotsLeft : Int } -> ( Weapon, Maybe Explosion )
shootAssaultBazooka direction position trail state =
    if state.shotsLeft <= 0 then
        ( AssaultBazooka state, Nothing )

    else
        ( AssaultBazooka { shotsLeft = state.shotsLeft - 1 }
        , Just
            (projectExplosionForwards
                direction
                position
                trail
                { center = vec2 0 0, size = 20, ticksLeft = 10 }
            )
        )


projectExplosionForwards : Direction -> Vec2 -> Trail -> Explosion -> Explosion
projectExplosionForwards direction position trail explosion =
    let
        move =
            { position = position
            , nextPosition = findArenaBorder direction position
            }

        collisionPoint =
            BikePhysics.collision move trail []
                |> Maybe.withDefault move.nextPosition
    in
    { explosion | center = collisionPoint }


findArenaBorder : Direction -> Vec2 -> Vec2
findArenaBorder direction position =
    let
        { x, y } =
            Vec2.toRecord position
    in
    case direction of
        North ->
            vec2 x 0

        South ->
            vec2 x C.h

        West ->
            vec2 0 y

        East ->
            vec2 C.w y
