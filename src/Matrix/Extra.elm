module Matrix.Extra exposing (decode, encode)

import Array exposing (Array)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Matrix exposing (Matrix)


encode : Matrix Int -> Value
encode matrix =
    Encode.object
        [ ( "width", Encode.int <| Matrix.width matrix )
        , ( "height", Encode.int <| Matrix.height matrix )
        , ( "data", Encode.array Encode.int (Matrix.toArray matrix) )
        ]


type alias Info =
    { width : Int
    , height : Int
    , data : Array Int
    }


decode : Decoder (Matrix Int)
decode =
    let
        infoDecoder : Decoder Info
        infoDecoder =
            Decode.map3 Info
                (Decode.field "width" Decode.int)
                (Decode.field "height" Decode.int)
                (Decode.field "data" <| Decode.array Decode.int)

        infoToMatrix : Info -> Matrix Int
        infoToMatrix info =
            info.data
                |> Array.toIndexedList
                |> List.foldl
                    (\( index, value ) matrix ->
                        Matrix.set
                            (remainderBy info.height index)
                            (index // info.width)
                            value
                            matrix
                    )
                    (Matrix.repeat info.width info.height 0)
    in
    Decode.map infoToMatrix infoDecoder
