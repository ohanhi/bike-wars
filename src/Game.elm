module Game exposing (..)

import AnimationFrame
import Bike
import Constants exposing (..)
import Direction exposing (..)
import Explosion
import Helpers
import Html exposing (..)
import Html.Attributes exposing (style)
import Keyboard.Extra exposing (Key(..))
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Trail
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
        ( 20, h / 2 )
        East
    , Bike.initBike { left = ArrowLeft, right = ArrowRight, up = ArrowUp }
        colors.green
        ( w - 20, h / 2 )
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
            let
                ( oldOne, oldTwo ) =
                    model.bikes

                trailForOne =
                    Bike.frontWall oldTwo
                        :: Bike.omitLastSections oldOne.trail
                        ++ Bike.cons oldTwo.position oldTwo.trail
                        ++ gameBounds

                trailForTwo =
                    Bike.frontWall oldOne
                        :: Bike.omitLastSections oldTwo.trail
                        ++ Bike.cons oldOne.position oldOne.trail
                        ++ gameBounds

                ( ( nextOne, expOne ), ( nextTwo, expTwo ) ) =
                    ( Bike.move diff trailForOne oldOne
                    , Bike.move diff trailForTwo oldTwo
                    )

                explosions =
                    model.explosions
                        |> Explosion.update
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

                ( one, two ) =
                    if model.status == Running then
                        ( nextOne, nextTwo )
                    else
                        ( oldOne, oldTwo )

                breakIfNecessary trail =
                    [ expOne, expTwo ]
                        |> List.filterMap identity
                        |> List.head
                        |> (\exp ->
                                case exp of
                                    Nothing ->
                                        trail

                                    Just explosion ->
                                        trail
                                            |> Helpers.trailToLines
                                            |> Trail.breakLines explosion
                                            |> Helpers.linesToTrail
                           )

                ( trailOne, trailTwo ) =
                    ( breakIfNecessary one.trail, breakIfNecessary two.trail )
            in
            { model
                | bikes =
                    ( { one | trail = trailOne }
                    , { two | trail = trailTwo }
                    )
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
                        { initModel | status = Running }
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
