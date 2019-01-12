module Game exposing (Bikes, EndState(..), GameStatus(..), Model, Msg(..), Player(..), init, initBikes, initModel, pureUpdate, stepGame, subscriptions, svgView, update, view)

import Bike
import Browser
import Browser.Events
import Constants exposing (..)
import Explosion
import Html exposing (..)
import Html.Attributes exposing (style)
import Keyboard exposing (Key(..))
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Types exposing (..)


type Player
    = PlayerOne
    | PlayerTwo


type EndState
    = GameWon Player
    | Draw


type GameStatus
    = NewGame
    | Running
    | GameOver EndState


type alias Bikes =
    ( Bike, Bike )


type alias Model =
    { bikes : Bikes
    , status : GameStatus
    , explosions : List Explosion
    }


type Msg
    = TimeDiff Float
    | KeyDown (Maybe Key)


initBikes : Bikes
initBikes =
    ( Bike.initBike
        { left = Character "a", right = Character "d", up = Character "w" }
        colors.red
        ( 20, h / 5 * 2 )
        East
    , Bike.initBike { left = ArrowLeft, right = ArrowRight, up = ArrowUp }
        colors.green
        ( w - 20, h / 5 * 3 )
        West
    )


initModel : Model
initModel =
    { bikes = initBikes
    , status = NewGame
    , explosions = []
    }


init : ( Model, Cmd Msg )
init =
    ( initModel
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        needsToAnimate =
            case ( model.status, model.explosions == [] ) of
                ( Running, _ ) ->
                    True

                ( GameOver _, False ) ->
                    True

                _ ->
                    False

        ticks =
            if needsToAnimate then
                [ Browser.Events.onAnimationFrameDelta TimeDiff ]

            else
                []
    in
    Sub.batch ([ Keyboard.downs (Keyboard.anyKey >> KeyDown) ] ++ ticks)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( pureUpdate msg model
    , Cmd.none
    )


pureUpdate : Msg -> Model -> Model
pureUpdate msg model =
    case msg of
        TimeDiff diff ->
            stepGame model diff

        KeyDown maybeKey ->
            maybeKey
                |> Maybe.map (handleInput model)
                |> Maybe.withDefault model


handleInput : Model -> Key -> Model
handleInput model key =
    case model.status of
        Running ->
            let
                ( one, two ) =
                    model.bikes

                ( oneUpdated, oneShot ) =
                    Bike.shoot key { current = two, other = one }

                ( twoUpdated, twoShot ) =
                    Bike.shoot key { current = one, other = two }

                shots =
                    [ oneShot, twoShot ]
                        |> List.filterMap identity
            in
            { model
                | bikes = ( Bike.turn key oneUpdated, Bike.turn key twoUpdated )
                , explosions = shots ++ model.explosions
            }

        _ ->
            if key == Character " " then
                { initModel | status = Running }

            else
                model


stepGame : Model -> Float -> Model
stepGame model diff =
    let
        ( oldOne, oldTwo ) =
            model.bikes

        explosions =
            model.explosions
                |> Explosion.update

        ( ( nextOne, expOne ), ( nextTwo, expTwo ) ) =
            ( Bike.update diff explosions { current = oldOne, other = oldTwo }
            , Bike.update diff explosions { current = oldTwo, other = oldOne }
            )

        nextExplosions =
            explosions
                |> List.append (List.filterMap identity [ expOne, expTwo ])

        allCollided =
            nextOne.collided && nextTwo.collided

        status =
            if allCollided then
                GameOver Draw

            else if nextOne.collided then
                GameOver (GameWon PlayerTwo)

            else if nextTwo.collided then
                GameOver (GameWon PlayerOne)

            else
                Running
    in
    { model
        | bikes = ( nextOne, nextTwo )
        , status = status
        , explosions = nextExplosions
    }


view : Model -> Html never
view model =
    div [ Html.Attributes.style "background-color" colors.grey ]
        [ svg
            [ viewBox ([ 0, 0, w, h ] |> List.map String.fromFloat |> String.join " ")
            , Html.Attributes.style "height" "100vh"
            , Html.Attributes.style "display" "block"
            , Html.Attributes.style "margin" "0 auto"
            ]
            (svgView model)
        ]


svgView : Model -> List (Svg never)
svgView model =
    let
        translucentRect =
            rect [ width (String.fromFloat w), height (String.fromFloat h), fill "rgba(0,0,0,0.5)" ] []

        makeOverlay string xOffset =
            [ translucentRect
            , text_
                [ x (String.fromFloat (w / 2))
                , y (String.fromFloat (h / 2))
                , fontSize "20"
                , fontFamily "monospace"
                , fill colors.white
                , transform ("translate(" ++ String.fromFloat xOffset ++ ", -10)")
                ]
                [ Svg.text string ]
            ]

        endOverlay state =
            case state of
                GameWon PlayerOne ->
                    makeOverlay "Red wins!" -50

                GameWon PlayerTwo ->
                    makeOverlay "Green wins!" -70

                Draw ->
                    makeOverlay "It's a draw." -70

        overlay status =
            case status of
                Running ->
                    []

                NewGame ->
                    makeOverlay "[SPACE], [⇦][⇨]" -90

                GameOver endState ->
                    endOverlay endState

        ( one, two ) =
            model.bikes
    in
    [ rect [ width (String.fromFloat w), height (String.fromFloat h), stroke colors.grey, strokeWidth (String.fromFloat bikeSize), fill colors.blue ] []
    , text_ [ x "10", y "30", fontSize "20", fontFamily "monospace", fill colors.white ] [ Svg.text "Bike Wars" ]
    ]
        ++ Bike.view one
        ++ Bike.view two
        ++ Explosion.view model.explosions
        ++ overlay model.status
