module Markov.Fuzz exposing (string)

import Fuzz exposing (Fuzzer)
import Markov.String exposing (MarkovString)


string : Fuzzer MarkovString
string =
    Fuzz.map
        (\data -> Markov.String.trainList data Markov.String.empty)
        (Fuzz.list Fuzz.string)
