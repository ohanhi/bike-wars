module Game exposing (Bikes, EndState(..), GameStatus(..), Model, Msg(..), Player(..), init, initBikes, initModel, pureUpdate, stepGame, subscriptions, svgView, update, view)

import Bike
import Browser
import Browser.Events
import Constants exposing (..)
import Explosion
import Html exposing (..)
import Html.Attributes exposing (style)
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import Math.Vector2 exposing (vec2)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Types exposing (..)
import Weapon


type Player
    = PlayerRed
    | PlayerGreen


type EndState
    = GameWon Player
    | Draw


type GameStatus
    = NewGame
    | Running
    | GameOver EndState


type alias Bikes =
    { red : Bike, green : Bike }


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
    { red =
        Bike.initBike
            { left = Character "a", right = Character "d", up = Character "w" }
            colors.red
            (vec2 20 (h / 5 * 2))
            East
            Weapon.initMegaBlaster
    , green =
        Bike.initBike { left = ArrowLeft, right = ArrowRight, up = ArrowUp }
            colors.green
            (vec2 (w - 20) (h / 5 * 3))
            West
            Weapon.initAssaultBazooka
    }


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

        keyParser =
            Keyboard.oneOf
                [ Keyboard.Arrows.arrowKey
                , Keyboard.whitespaceKey
                ]
    in
    Sub.batch ([ Keyboard.downs (keyParser >> KeyDown) ] ++ ticks)


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
                { red, green } =
                    model.bikes

                ( redUpdated, redShot ) =
                    Bike.shoot key { current = red, other = green }

                ( greenUpdated, greenShot ) =
                    Bike.shoot key { current = green, other = red }

                shots =
                    [ redShot, greenShot ]
                        |> List.filterMap identity
            in
            { model
                | bikes = { red = Bike.turn key redUpdated, green = Bike.turn key greenUpdated }
                , explosions = shots ++ model.explosions
            }

        _ ->
            if key == Spacebar then
                { initModel | status = Running }

            else
                model


stepGame : Model -> Float -> Model
stepGame ({ bikes } as model) diff =
    let
        explosions =
            model.explosions
                |> Explosion.update

        ( ( nextRed, expRed ), ( nextGreen, expGreen ) ) =
            ( Bike.update diff explosions { current = bikes.red, other = bikes.green }
            , Bike.update diff explosions { current = bikes.green, other = bikes.red }
            )

        nextExplosions =
            explosions
                |> List.append (List.filterMap identity [ expRed, expGreen ])

        allCollided =
            nextRed.collided && nextGreen.collided

        status =
            if allCollided then
                GameOver Draw

            else if nextRed.collided then
                GameOver (GameWon PlayerGreen)

            else if nextGreen.collided then
                GameOver (GameWon PlayerRed)

            else
                Running
    in
    { model
        | bikes = { red = nextRed, green = nextGreen }
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
                GameWon PlayerRed ->
                    makeOverlay "Red wins!" -50

                GameWon PlayerGreen ->
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
    in
    [ rect [ width (String.fromFloat w), height (String.fromFloat h), stroke colors.grey, strokeWidth (String.fromFloat bikeSize), fill colors.blue ] []
    , text_ [ x "10", y "30", fontSize "20", fontFamily "monospace", fill colors.white ] [ Svg.text "Bike Wars" ]
    ]
        ++ Bike.view model.bikes.red
        ++ Bike.view model.bikes.green
        ++ Explosion.view model.explosions
        ++ overlay model.status
