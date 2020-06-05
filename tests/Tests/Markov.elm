module Tests.Markov exposing (..)

import Expect
import Fuzz
import Markov
import Markov.Fuzz as Fuzz
import Test exposing (Test, fuzz3, test)


getSimpleElementProperties : Test
getSimpleElementProperties =
    test "Get probability of 1.0 from a -> a" <|
        \_ ->
            Markov.empty
                |> Markov.add 'a' 'a'
                |> Markov.probabilityOf 'a' 'a'
                |> Expect.equal 1


randomGet : Test
randomGet =
    fuzz3 Fuzz.markov Fuzz.char Fuzz.char "Probability should always less than one" <|
        \markov from to ->
            Markov.probabilityOf from to markov
                |> Expect.all
                    [ Expect.atLeast 0
                    , Expect.atMost 1
                    ]
