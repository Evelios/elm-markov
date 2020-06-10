module Tests.Matrix.Extra exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode
import Matrix exposing (Matrix)
import Matrix.Extra
import Test exposing (Test, fuzz)


fuzzSmall =
    Fuzz.intRange 0 100


fuzzMatrix : Fuzzer (Matrix Int)
fuzzMatrix =
    Fuzz.map2 (\w h -> Matrix.repeat w h 0) fuzzSmall fuzzSmall


encodeAndDecode : Test
encodeAndDecode =
    fuzz fuzzMatrix "Encode and decode back to same matrix" <|
        \matrix ->
            Json.Decode.decodeValue Matrix.Extra.decode (Matrix.Extra.encode matrix)
                |> Expect.equal (Ok matrix)
