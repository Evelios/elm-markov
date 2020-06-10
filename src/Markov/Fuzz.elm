module Markov.Fuzz exposing (markov)

import Fuzz exposing (Fuzzer)
import Markov exposing (Markov)


markov : Fuzzer Markov
markov =
    Fuzz.map
        (\data -> Markov.addList data Markov.empty)
        (Fuzz.list Fuzz.string)
