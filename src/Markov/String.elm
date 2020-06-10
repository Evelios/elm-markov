module Markov.String exposing
    ( MarkovString
    , empty
    , train, trainList
    , encode, decode
    )

{-| Train a markov model for string generation. This module provides some wrapper functionality around the markov
module so that you can do word generation. Sentence generation using markov models is fairly straight forward by using
lists of `String`s as training data. Just like how the core `String` module makes some character operations easier this
section helps to wrap `List Char` operations into a more simpler `String` interface. This module contains only the
operations that benefit from simplification not covered by the core library. This sections also serves as a good
template on how to extend the markov package for use in your own data.


# Types

@docs MarkovString


# Builders

@docs empty


# Modifiers

@docs train, trainList


# Json

@docs encode, decode

-}

import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Markov exposing (Element(..), Markov)


type alias MarkovString =
    Markov Int Char


charComparable : Element Char -> Int
charComparable element =
    case element of
        Start ->
            -2

        Element c ->
            Char.toCode c

        End ->
            -1


empty : MarkovString
empty =
    Markov.empty charComparable


train : String -> MarkovString -> MarkovString
train string markov =
    Markov.train (String.toList string) markov


trainList : List String -> MarkovString -> MarkovString
trainList strings markov =
    List.foldl train markov strings



-- Json


encode : MarkovString -> Value
encode =
    Markov.encode String.fromChar


decode : Decoder MarkovString
decode =
    let
        stringToChar string =
            string
                |> String.toList
                |> List.head
                |> Maybe.withDefault (Char.fromCode 0)
    in
    Markov.decode stringToChar charComparable
