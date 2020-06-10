module Tests.Markov exposing (..)

import Expect
import Json.Decode
import Markov.Fuzz
import Markov.String
import Test exposing (Test, fuzz)


encodeAndDecode : Test
encodeAndDecode =
    fuzz Markov.Fuzz.string "Encode and decode markov model" <|
        \markov ->
            Json.Decode.decodeValue Markov.String.decode (Markov.String.encode markov)
                |> Expect.equal (Ok markov)
