module Game exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Math.Vector2 as Vec2 exposing (Vec2, vec2, getX, getY)
import AnimationFrame
import Keyboard.Extra as KeyExtra exposing (Key(..))
import Direction exposing (..)


speedC : Float
speedC =
    0.1


w : Float
w =
    400


h : Float
h =
    300


gameBounds : List Line
gameBounds =
    [ Horizontal ( vec2 0 0, vec2 w 0 )
    , Horizontal ( vec2 0 h, vec2 w h )
    , Vertical ( vec2 0 0, vec2 0 h )
    , Vertical ( vec2 w 0, vec2 w h )
    ]


type alias Model =
    { position : Vec2
    , trail : List Vec2
    , collided : Bool
    , direction : Direction
    }


type Msg
    = TimeDiff Float
    | KeyDown Key


init : ( Model, Cmd Msg )
init =
    ( { position = vec2 20 20
      , trail = [ vec2 20 20 ]
      , collided = False
      , direction = East
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        ticks =
            if not model.collided then
                [ AnimationFrame.diffs TimeDiff ]
            else
                []
    in
        Sub.batch ([ KeyExtra.downs KeyDown ] ++ ticks)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    pureUpdate msg model ! []


pureUpdate : Msg -> Model -> Model
pureUpdate msg model =
    case msg of
        TimeDiff diff ->
            { model
                | position =
                    if model.collided then
                        model.position
                    else
                        computePosition model.direction model.position diff
                , collided =
                    isCollision model.direction model.position model.trail
            }

        KeyDown key ->
            if model.collided then
                model
            else
                { model
                    | direction = computeDirection model.direction key
                    , trail = model.position :: model.trail
                }


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


computeDirection : Direction -> Key -> Direction
computeDirection direction key =
    case key of
        ArrowLeft ->
            turnLeft direction

        ArrowRight ->
            turnRight direction

        _ ->
            direction


type Line
    = Horizontal ( Vec2, Vec2 )
    | Vertical ( Vec2, Vec2 )


vecListToLines : List Line -> List Vec2 -> List Line
vecListToLines acc list =
    case list of
        a :: b :: tail ->
            vecListToLines ((orderedTuple a b) :: acc) (b :: tail)

        _ ->
            acc


orderedTuple : Vec2 -> Vec2 -> Line
orderedTuple a b =
    let
        (( aX, aY ) as tupleA) =
            Vec2.toTuple a

        (( bX, bY ) as tupleB) =
            Vec2.toTuple b
    in
        if aX == bX then
            -- vertical
            if aY < bY then
                Vertical ( a, b )
            else
                Vertical ( b, a )
        else if aX < bX then
            -- horizontal
            Horizontal ( a, b )
        else
            Horizontal ( b, a )


reduceVertical : Float -> Line -> Maybe Vec2
reduceVertical posY line =
    case line of
        Vertical ( a, b ) ->
            if (getY a < posY) && (getY b > posY) then
                Just (vec2 (getX a) posY)
            else
                Nothing

        _ ->
            Nothing


reduceHorizontal : Float -> Line -> Maybe Vec2
reduceHorizontal posX line =
    case line of
        Horizontal ( a, b ) ->
            if (getX a < posX) && (getX b > posX) then
                Just (vec2 posX (getY a))
            else
                Nothing

        _ ->
            Nothing


isCollision : Direction -> Vec2 -> List Vec2 -> Bool
isCollision direction pos linePoints =
    let
        ( posX, posY ) =
            Vec2.toTuple pos

        lines =
            gameBounds ++ (vecListToLines [] linePoints)

        verticals =
            List.filterMap (reduceVertical posY) lines

        horizontals =
            List.filterMap (reduceHorizontal posX) lines

        compareTo =
            (List.map (Vec2.distance pos)) >> (List.any lessThanEpsilon)

        lessThanEpsilon x =
            x < 5
    in
        case direction of
            North ->
                compareTo horizontals

            South ->
                compareTo horizontals

            East ->
                compareTo verticals

            West ->
                compareTo verticals


view : Model -> Html never
view model =
    div []
        [ svg
            [ width (toString (2 * w))
            , height (toString (2 * h))
            , viewBox ([ 0, 0, w, h ] |> List.map toString |> String.join " ")
            , Html.Attributes.style [ ( "float", "left" ) ]
            ]
            (svgView model)
        , table []
            [ tr []
                [ td [] [ Html.text "x" ]
                , td [] [ Html.text (toString (Vec2.getX model.position)) ]
                ]
            , tr []
                [ td [] [ Html.text "y" ]
                , td [] [ Html.text (toString (Vec2.getY model.position)) ]
                ]
            , tr []
                [ td [] [ Html.text "dir" ]
                , td [] [ Html.text (toString model.direction) ]
                ]
            ]
        ]


svgView : Model -> List (Svg never)
svgView model =
    let
        bikeSize =
            6

        posX =
            Vec2.getX model.position

        posY =
            Vec2.getY model.position

        rotation =
            case model.direction of
                North ->
                    "0"

                East ->
                    "90"

                South ->
                    "180"

                West ->
                    "270"

        transformValue =
            "translate(" ++ toString (posX - bikeSize) ++ " " ++ toString (posY - bikeSize) ++ ") rotate(" ++ rotation ++ " " ++ toString bikeSize ++ " " ++ toString bikeSize ++ ")"

        trailPoints =
            (model.position :: model.trail)
                |> List.map (\vec -> toString (Vec2.getX vec) ++ "," ++ toString (Vec2.getY vec))
                |> String.join " "

        overlay =
            if model.collided then
                [ rect [ width (toString w), height (toString h), fill "rgba(0,0,0,0.5)" ] []
                , text_
                    [ x (toString (w / 2))
                    , y (toString (h / 2))
                    , fontSize "20"
                    , fontFamily "monospace"
                    , fill "white"
                    , transform "translate(-30, -10)"
                    ]
                    [ Svg.text "DEAD!" ]
                ]
            else
                []
    in
        [ rect [ width (toString w), height (toString h), stroke "gray", strokeWidth "6", fill "blue" ] []
        , text_ [ x "10", y "30", fontSize "20", fontFamily "monospace", fill "white" ] [ Svg.text "Bike Wars" ]
        , polyline [ fill "none", stroke "red", strokeWidth "3", points trailPoints ] []
        , image
            [ width (toString (2 * bikeSize))
            , height (toString (2 * bikeSize))
            , xlinkHref "img/bike.svg"
            , transform (transformValue)
            ]
            []
        ]
            ++ overlay
