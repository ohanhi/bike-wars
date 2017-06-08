module Game exposing (..)

import AnimationFrame
import Bike
import Constants exposing (..)
import Direction exposing (..)
import Explosion
import Html exposing (..)
import Html.Attributes exposing (style)
import Keyboard.Extra exposing (Key(..))
import Math.Vector2 exposing (vec2)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Types exposing (..)


type Player
    = PlayerOne
    | PlayerTwo


type GameStatus
    = NewGame
    | Running
    | GameWon Player
    | Draw


type alias Bikes =
    ( Bike, Bike )


type alias Model =
    { bikes : Bikes
    , status : GameStatus
    , explosions : List Explosion
    }


type Msg
    = TimeDiff Float
    | KeyDown Key


initBikes : Bikes
initBikes =
    ( Bike.initBike
        { left = CharA, right = CharD, up = CharW }
        colors.red
        ( 20, h / 2 )
        East
    , Bike.initBike { left = ArrowLeft, right = ArrowRight, up = ArrowUp }
        colors.green
        ( w - 20, h / 2 )
        West
    )


initExplosions : List Explosion
initExplosions =
    [ { center = vec2 (w / 2) (h / 7)
      , size = 40
      , ticksLeft = 5 * ticksPerSecond
      }
    , { center = vec2 40 40
      , size = 20
      , ticksLeft = 2 * ticksPerSecond
      }
    ]


init : ( Model, Cmd Msg )
init =
    ( { bikes = initBikes
      , status = NewGame
      , explosions = initExplosions
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
                explosions =
                    Explosion.update model.explosions

                ( oldOne, oldTwo ) =
                    model.bikes

                compoundTrail =
                    Bike.cons oldOne.position oldOne.trail
                        ++ Bike.cons oldTwo.position oldTwo.trail
                        ++ Explosion.toTrail explosions

                (( one, two ) as newBikes) =
                    ( Bike.move diff (Bike.frontWall oldTwo :: compoundTrail) oldOne
                    , Bike.move diff (Bike.frontWall oldOne :: compoundTrail) oldTwo
                    )

                allCollided =
                    one.collided && two.collided

                status =
                    if allCollided then
                        Draw
                    else if one.collided then
                        GameWon PlayerTwo
                    else if two.collided then
                        GameWon PlayerOne
                    else
                        Running
            in
            { model
                | bikes = newBikes
                , status = status
                , explosions = explosions
            }

        KeyDown key ->
            case model.status of
                Running ->
                    let
                        ( one, two ) =
                            model.bikes
                    in
                    { model
                        | bikes = ( Bike.turn key one, Bike.turn key two )
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
    div [ Html.Attributes.style [ ( "background-color", colors.grey ) ] ]
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

        makeOverlay string xOffset =
            [ translucentRect
            , text_
                [ x (toString (w / 2))
                , y (toString (h / 2))
                , fontSize "20"
                , fontFamily "monospace"
                , fill colors.white
                , transform ("translate(" ++ toString xOffset ++ ", -10)")
                ]
                [ Svg.text string ]
            ]

        overlay =
            case model.status of
                GameWon PlayerOne ->
                    makeOverlay "Red wins!" -50

                GameWon PlayerTwo ->
                    makeOverlay "Green wins!" -70

                Draw ->
                    makeOverlay "It's a draw." -70

                NewGame ->
                    makeOverlay "[SPACE], [⇦][⇨]" -90

                _ ->
                    []

        ( one, two ) =
            model.bikes
    in
    [ rect [ width (toString w), height (toString h), stroke colors.grey, strokeWidth (toString bikeSize), fill colors.blue ] []
    , text_ [ x "10", y "30", fontSize "20", fontFamily "monospace", fill colors.white ] [ Svg.text "Bike Wars" ]
    ]
        ++ Bike.view one
        ++ Bike.view two
        ++ Explosion.view model.explosions
        ++ overlay
