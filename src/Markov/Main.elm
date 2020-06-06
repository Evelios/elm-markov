module Markov.Main exposing (..)

import Browser
import Html exposing (Html)


type Model
    = Model


type Msg
    = Msg


main : Program () Msg Model
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update _ _ =
    ( Model, Cmd.none )


view : Model -> Html Msg
view _ =
    Html.div [] []
