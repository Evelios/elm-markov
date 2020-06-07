module Main exposing (..)

import Browser
import File exposing (File)
import File.Select
import Html exposing (Html)
import Html.Events
import Markov exposing (Markov)
import Random
import Task


type alias Model =
    { words : List String
    , markov : Markov
    }


type Msg
    = NewWord String
    | RequestCorpus
    | CorpusFileLoaded File
    | CorpusLoaded String
    | GenerateWord
    | WordGenerated String


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { words = []
      , markov = Markov.empty
      }
    , Cmd.none
    )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewWord word ->
            ( { model | words = word :: model.words }
            , Cmd.none
            )

        RequestCorpus ->
            ( model, requestCorpus )

        CorpusFileLoaded file ->
            ( model
            , Task.perform CorpusLoaded <| File.toString file
            )

        CorpusLoaded corpus ->
            { model | markov = trainMarkov corpus }
                |> update GenerateWord

        GenerateWord ->
            ( model
            , Random.generate WordGenerated <| Markov.word model.markov
            )

        WordGenerated word ->
            ( { model | words = word :: model.words }
            , Cmd.none
            )


requestCorpus : Cmd Msg
requestCorpus =
    File.Select.file [ "text" ] CorpusFileLoaded


trainMarkov : String -> Markov
trainMarkov rawCorpus =
    let
        cleanCorpus : List String
        cleanCorpus =
            rawCorpus
                |> String.toLower
                |> String.words
                |> List.map (String.filter Char.isAlpha)
                |> List.filter (not << String.isEmpty)
    in
    cleanCorpus
        |> (\cleanedCorpus -> Markov.addList cleanedCorpus Markov.empty)



-- View


view : Model -> Html Msg
view model =
    let
        paragraphs =
            model.words
                |> List.map
                    (\w ->
                        Html.p [] [ Html.text w ]
                    )

        buttons =
            [ Html.button [ Html.Events.onClick RequestCorpus ] [ Html.text "Load corpus" ]
            , Html.button [ Html.Events.onClick GenerateWord ] [ Html.text "Generate Word" ]
            ]
    in
    Html.div [] (List.append buttons paragraphs)
