module Game exposing (..)

import AnimationFrame
import Bike
import Constants exposing (..)
import Explosion
import Html exposing (..)
import Html.Attributes exposing (style)
import Keyboard.Extra exposing (Key(..))
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
    | KeyDown Key


initBikes : Bikes
initBikes =
    ( Bike.initBike
        { left = CharA, right = CharD, up = CharW }
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
            stepGame model diff

        KeyDown key ->
            case model.status of
                Running ->
                    let
                        ( one, two ) =
                            model.bikes

                        shots =
                            Bike.shoot key { current = one, other = two }
                                ++ Bike.shoot key { current = two, other = one }
                    in
                    { model
                        | bikes = ( Bike.turn key one, Bike.turn key two )
                        , explosions = shots ++ model.explosions
                    }

                _ ->
                    if key == Space then
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
    [ rect [ width (toString w), height (toString h), stroke colors.grey, strokeWidth (toString bikeSize), fill colors.blue ] []
    , text_ [ x "10", y "30", fontSize "20", fontFamily "monospace", fill colors.white ] [ Svg.text "Bike Wars" ]
    ]
        ++ Bike.view one
        ++ Bike.view two
        ++ Explosion.view model.explosions
        ++ overlay model.status
