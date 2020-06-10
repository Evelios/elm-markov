module Tests.Markov exposing (..)

import Expect
import Json.Decode
import Markov
import Markov.Fuzz
import Test exposing (Test, fuzz)


encodeAndDecode : Test
encodeAndDecode =
    fuzz Markov.Fuzz.markov "Encode and decode markov model" <|
        \markov ->
            Json.Decode.decodeValue Markov.decode (Markov.encode markov)
                |> Expect.equal (Ok markov)
