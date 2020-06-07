module Tests.List.Util exposing (..)

import Expect
import List.Util
import Test exposing (Test, describe, test)


groupsOfTwo : Test
groupsOfTwo =
    describe "groupsOfTwo"
        [ test "Empty list" <|
            \_ ->
                List.Util.groupsOfTwo []
                    |> Expect.equal []
        , test "Single element" <|
            \_ ->
                List.Util.groupsOfTwo [ 1 ]
                    |> Expect.equal []
        , test "Two elements" <|
            \_ ->
                List.Util.groupsOfTwo [ 1, 2 ]
                    |> Expect.equal [ ( 1, 2 ) ]
        , test "Three elements" <|
            \_ ->
                List.Util.groupsOfTwo [ 1, 2, 3 ]
                    |> Expect.equal [ ( 1, 2 ), ( 2, 3 ) ]
        ]
