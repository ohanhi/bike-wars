module BikePhysics exposing (collision, computeDirection, computePosition)

{-| Doctest imports

    import Math.Vector2 exposing (vec2)
    import Direction exposing (..)

-}

import Constants exposing (..)
import Direction exposing (..)
import Helpers exposing (..)
import Keyboard exposing (Key(..))
import Math.Vector2 as Vec2 exposing (Vec2, getX, getY, vec2)
import Types exposing (..)


computePosition : Direction -> Vec2 -> Float -> Vec2
computePosition direction position diff =
    let
        c =
            speedC * diff

        change =
            case direction of
                North ->
                    vec2 0 -c

                South ->
                    vec2 0 c

                East ->
                    vec2 c 0

                West ->
                    vec2 -c 0
    in
    Vec2.add position change


computeDirection : Controls -> Direction -> Key -> Direction
computeDirection controls direction key =
    if key == controls.left then
        turnLeft direction

    else if key == controls.right then
        turnRight direction

    else
        direction


{-| This function should find any sort of collision with the current walls or explosions.
-}
collision : { position : Vec2, nextPosition : Vec2 } -> Trail -> List Obstacle -> Maybe Vec2
collision { position, nextPosition } trail obstacles =
    let
        lines =
            trailToLines trail

        moveLine =
            toLine position nextPosition

        try : Maybe Vec2 -> Maybe Vec2 -> Maybe Vec2
        try maybeOne maybeTwo =
            case maybeOne of
                Just collision_ ->
                    Just collision_

                Nothing ->
                    maybeTwo
    in
    case moveLine of
        Just (Horizontal points) ->
            try
                (obstacles
                    |> List.filterMap (tryObstacleCollision points)
                    |> List.head
                )
                (lines
                    |> List.filterMap onlyVertical
                    |> tryCollision (horizontalOrthogonal points)
                )

        Just (Vertical points) ->
            try
                (obstacles
                    |> List.filterMap (tryObstacleCollision points)
                    |> List.head
                )
                (lines
                    |> List.filterMap onlyHorizontal
                    |> tryCollision (verticalOrthogonal points)
                )

        Nothing ->
            Nothing
