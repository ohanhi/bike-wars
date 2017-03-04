module Game exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Math.Vector2 as Vec2 exposing (Vec2, vec2, getX, getY)
import AnimationFrame
import Keyboard.Extra exposing (Key(..))
import Types exposing (Bike)
import Bike exposing (..)
import Constants exposing (..)


type GameStatus
    = NewGame
    | Running
    | GameOver


type alias Model =
    { bike : Bike
    , status : GameStatus
    }


type Msg
    = TimeDiff Float
    | KeyDown Key


init : ( Model, Cmd Msg )
init =
    ( { bike = initBike
      , status = NewGame
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        ticks =
            if model.status == Running then
                [ AnimationFrame.diffs TimeDiff ]
            else
                []
    in
        Sub.batch ([ Keyboard.Extra.downs KeyDown ] ++ ticks)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    pureUpdate msg model ! []


pureUpdate : Msg -> Model -> Model
pureUpdate msg model =
    case msg of
        TimeDiff diff ->
            let
                newBike =
                    Bike.move diff model.bike
            in
                { model
                    | bike = newBike
                    , status =
                        if newBike.collided then
                            GameOver
                        else
                            Running
                }

        KeyDown key ->
            case model.status of
                Running ->
                    { model
                        | bike = Bike.turn key model.bike
                    }

                _ ->
                    if key == Space then
                        { model
                            | bike = Bike.initBike
                            , status = Running
                        }
                    else
                        model


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
                , td [] [ Html.text (toString (getX model.bike.position)) ]
                ]
            , tr []
                [ td [] [ Html.text "y" ]
                , td [] [ Html.text (toString (getY model.bike.position)) ]
                ]
            , tr []
                [ td [] [ Html.text "dir" ]
                , td [] [ Html.text (toString model.bike.direction) ]
                ]
            ]
        ]


svgView : Model -> List (Svg never)
svgView model =
    let
        translucentRect =
            rect [ width (toString w), height (toString h), fill "rgba(0,0,0,0.5)" ] []

        overlay =
            case model.status of
                GameOver ->
                    [ translucentRect
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

                NewGame ->
                    [ translucentRect
                    , text_
                        [ x (toString (w / 2))
                        , y (toString (h / 2))
                        , fontSize "20"
                        , fontFamily "monospace"
                        , fill "white"
                        , transform "translate(-100, -10)"
                        ]
                        [ Svg.text "[SPACE], [⇦][⇨]" ]
                    ]

                _ ->
                    []
    in
        [ rect [ width (toString w), height (toString h), stroke "gray", strokeWidth "6", fill "blue" ] []
        , text_ [ x "10", y "30", fontSize "20", fontFamily "monospace", fill "white" ] [ Svg.text "Bike Wars" ]
        ]
            ++ Bike.view model.bike
            ++ overlay
