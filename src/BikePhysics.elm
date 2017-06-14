module BikePhysics exposing (collision, computeDirection, computePosition)

{-| Doctest imports

    >>> import Math.Vector2 exposing (vec2)
    >>> import Direction exposing (..)

-}

import Constants exposing (..)
import Direction exposing (..)
import Helpers exposing (..)
import Keyboard.Extra exposing (Key(..))
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


{-| This function should find any sort of collision with the current walls.

Vertical line, moving horizontally

    >>> collision
    ... { position = vec2 0 0, nextPosition = vec2 -10 0 }
    ... [[ vec2 1 -10, vec2 1 10 ]] -- West
    Nothing
    >>> collision
    ... { position = vec2 0 0, nextPosition = vec2 10 0 }
    ... [[ vec2 1 -10, vec2 1 10 ]] -- East
    Just (vec2 1 0)
    >>> collision
    ... { position = vec2 10 0, nextPosition = vec2 0 0 }
    ... [[ vec2 1 -10, vec2 1 10 ]] -- West
    Just (vec2 1 0)
    >>> collision
    ... { position = vec2 0 0, nextPosition = vec2 5 0 }
    ... [[ vec2 10 -10, vec2 10 10 ]] -- East
    Nothing

Horizontal line, moving vertically

    >>> collision
    ... { position = vec2 0 0, nextPosition = vec2 0 10 }
    ... [[ vec2 -10 1, vec2 10 1 ]]
    Just (vec2 0 1)
    >>> collision
    ... { position = vec2 0 0, nextPosition = vec2 0 -10 }
    ... [[ vec2 -10 1, vec2 10 1 ]]
    Nothing

Horizontal line, moving horizontally

    >>> collision
    ... { position = vec2 0 0, nextPosition = vec2 10 0 }
    ... [[ vec2 -10 1, vec2 10 1 ]]
    Nothing

Vertical line, moving vertically

    >>> collision
    ... { position = vec2 0 0, nextPosition = vec2 0 10 }
    ... [[ vec2 1 -10, vec2 1 10 ]] -- North
    Nothing

-}
collision : { position : Vec2, nextPosition : Vec2 } -> Trail -> Maybe Vec2
collision { position, nextPosition } trail =
    let
        lines =
            trailToLines trail

        moveLine =
            toLine position nextPosition
    in
    case moveLine of
        Horizontal points ->
            lines
                |> List.filterMap onlyVertical
                |> tryCollision (horizontalOrthogonal points)

        Vertical points ->
            lines
                |> List.filterMap onlyHorizontal
                |> tryCollision (verticalOrthogonal points)
