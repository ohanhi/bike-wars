module Main exposing (main)

import Browser
import Game exposing (..)


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , update = update
        , view =
            \model ->
                { title = "Bike Wars"
                , body = [ view model ]
                }
        , subscriptions = subscriptions
        }
