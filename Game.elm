module Game exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import AnimationFrame
import Keyboard.Extra as KeyExtra exposing (Key(..))
import Direction exposing (..)


speedC : Float
speedC =
    0.1


type alias Model =
    { position : Vec2
    , trail : List Vec2
    , direction : Direction
    }


type Msg
    = TimeDiff Float
    | KeyDown Key


init : ( Model, Cmd Msg )
init =
    ( { position = vec2 20 20
      , trail = [ vec2 20 20 ]
      , direction = East
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ KeyExtra.downs KeyDown
        , AnimationFrame.diffs TimeDiff
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    pureUpdate msg model ! []


pureUpdate : Msg -> Model -> Model
pureUpdate msg model =
    case msg of
        TimeDiff diff ->
            { model | position = computePosition model.direction model.position diff }

        KeyDown key ->
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


view : Model -> Html never
view model =
    div []
        [ svg
            [ width "800"
            , height "600"
            , viewBox "0 0 400 300"
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
    in
        [ rect [ width "400", height "300", stroke "gray", strokeWidth "6", fill "blue" ] []
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
