module Markov.Fuzz exposing (markov)

import Fuzz exposing (Fuzzer)
import Markov exposing (Markov)


alphabet : List Char
alphabet =
    List.range (Char.toCode 'a') (Char.toCode 'z')
        |> List.map Char.fromCode


markov : Fuzzer Markov
markov =
    Fuzz.map
        (\data -> Markov.addList data (Markov.empty alphabet))
        (Fuzz.list Fuzz.string)
