module Main exposing (Model, Msg(..), init, main, subscriptions, trainMarkov, update, view)

import Browser
import File exposing (File)
import File.Download
import File.Select
import Html exposing (Html)
import Html.Events
import Json.Decode
import Json.Encode
import Markov
import Markov.String exposing (MarkovString)
import Random
import Task


type alias Model =
    { words : List String
    , markov : MarkovString
    }


type Msg
    = NewWord String
    | RequestCorpus
    | CorpusFileLoaded File
    | CorpusLoaded String
    | GenerateWord
    | WordGenerated String
    | DownloadMarkov
    | RequestMarkov
    | MarkovFileLoaded File
    | MarkovLoaded String


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { words = []
      , markov = Markov.String.empty
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewWord word ->
            ( { model | words = word :: model.words }
            , Cmd.none
            )

        RequestCorpus ->
            ( model
            , File.Select.file [ "text" ] CorpusFileLoaded
            )

        CorpusFileLoaded file ->
            ( model
            , Task.perform CorpusLoaded <| File.toString file
            )

        CorpusLoaded corpus ->
            { model | markov = trainMarkov corpus }
                |> update GenerateWord

        GenerateWord ->
            ( model
            , Random.generate WordGenerated <|
                Random.map String.fromList
                    (Markov.phrase
                        { maxLength = 10
                        }
                        model.markov
                    )
            )

        WordGenerated word ->
            ( { model | words = word :: model.words }
            , Cmd.none
            )

        DownloadMarkov ->
            ( model
            , File.Download.string "markov.json" "text/json" <|
                Json.Encode.encode 2 <|
                    Markov.String.encode model.markov
            )

        RequestMarkov ->
            ( model
            , File.Select.file [ "text/json" ] MarkovFileLoaded
            )

        MarkovFileLoaded file ->
            ( model
            , Task.perform MarkovLoaded <| File.toString file
            )

        MarkovLoaded json ->
            { model
                | markov =
                    Json.Decode.decodeString Markov.String.decode json
                        |> Result.withDefault model.markov
            }
                |> update GenerateWord


trainMarkov : String -> MarkovString
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
        |> (\cleanedCorpus -> Markov.String.trainList cleanedCorpus Markov.String.empty)



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
            , Html.button [ Html.Events.onClick DownloadMarkov ] [ Html.text "Download Model" ]
            , Html.button [ Html.Events.onClick RequestMarkov ] [ Html.text "Load Model" ]
            ]
    in
    Html.div [] (List.append buttons paragraphs)
