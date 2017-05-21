module Game exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame
import Keyboard.Extra exposing (Key(..))
import Types exposing (Bike)
import Bike exposing (..)
import Constants exposing (..)
import Direction exposing (..)
import Types exposing (Controls)


type GameStatus
    = NewGame
    | Running
    | GameOver


type alias Model =
    { bikes :
        { one : Bike
        , two : Bike
        }
    , status : GameStatus
    }


type Msg
    = TimeDiff Float
    | KeyDown Key


bikeColors : { one : String, two : String }
bikeColors =
    { one = "red"
    , two = "yellow"
    }


bikeControls : { one : Controls, two : Controls }
bikeControls =
    { one = { left = CharA, right = CharD, up = CharW }
    , two = { left = ArrowLeft, right = ArrowRight, up = ArrowUp }
    }


initBikes : { one : Bike, two : Bike }
initBikes =
    { one = initBike bikeControls.one bikeColors.one ( 20, h / 2 ) East
    , two = initBike bikeControls.two bikeColors.two ( w - 20, h / 2 ) West
    }


init : ( Model, Cmd Msg )
init =
    ( { bikes = initBikes
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
                compoundTrail =
                    model.bikes.one.trail ++ model.bikes.two.trail

                move =
                    Bike.move diff compoundTrail

                newBikes =
                    { one = move model.bikes.one
                    , two = move model.bikes.two
                    }

                anyCollided =
                    newBikes.one.collided || newBikes.two.collided

                status =
                    if anyCollided then
                        GameOver
                    else
                        Running
            in
                { model
                    | bikes = newBikes
                    , status = status
                }

        KeyDown key ->
            case model.status of
                Running ->
                    { model
                        | bikes =
                            { one = Bike.turn key model.bikes.one
                            , two = Bike.turn key model.bikes.two
                            }
                    }

                _ ->
                    if key == Space then
                        { model
                            | bikes = initBikes
                            , status = Running
                        }
                    else
                        model


view : Model -> Html never
view model =
    div [ Html.Attributes.style [ ( "background-color", "#222" ) ] ]
        [ svg
            [ viewBox ([ 0, 0, w, h ] |> List.map toString |> String.join " ")
            , Html.Attributes.style
                [ ( "height", "100vh" )
                , ( "display", "block" )
                , ( "margin", "0 auto" )
                ]
            ]
            (svgView model)
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
            ++ Bike.view model.bikes.one
            ++ Bike.view model.bikes.two
            ++ overlay
