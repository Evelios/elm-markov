module Markov.Fuzz exposing (markov)

import Fuzz exposing (Fuzzer)
import Markov exposing (Markov)


markov : Fuzzer Markov
markov =
    Fuzz.map
        (List.foldl (\( from, to ) -> Markov.add from to) Markov.empty)
        transitions


transitions : Fuzzer (List ( Char, Char ))
transitions =
    Fuzz.list (Fuzz.tuple ( Fuzz.char, Fuzz.char ))
