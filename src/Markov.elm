module Markov exposing
    ( Markov
    , add, addList
    , word
    , empty, probabilityOf
    )

{-| Create a markov transition model of string inputs. This creates


# Types

@docs Markov


# Builders

@docs fromList


# Modifiers

@docs add, addList


# Generation

@docs word

-}

import Dict exposing (Dict)
import List.Extra
import Matrix exposing (Matrix)
import Random exposing (Generator)



{- A markov graph which represents the chance of transitioning from one element to another.

   The row is the 'from' element and the column is the 'to' element.

   Example:

   Total = 15
       a  b  c
     a 1  2  1
     b 3  2  5
     c 0  1  0

   Chance of a -> b = 2 / 15
   Chance of b -> a = 3 / 15
-}


type Markov
    = Markov (Matrix Int) Int



-- Builders


{-| Create an empty markov chain with no elements in it. This is needed to add further elements into it to start to
train the model.
-}
empty : Markov
empty =
    let
        numCharacters =
            36
    in
    Markov (Matrix.repeat numCharacters numCharacters 0) 0



-- Accessors


{-| Private: The list of all possible character elements that will be stored in the markov model.
-}
elements : List Char
elements =
    let
        lowercase =
            List.range (Char.toCode 'a') (Char.toCode 'z')
                |> List.map Char.fromCode

        numbers =
            List.range (Char.toCode '0') (Char.toCode '9')
                |> List.map Char.fromCode
    in
    List.concat
        [ lowercase
        , numbers
        , [ ' ' ]
        ]


{-| Private: The dictionary of mappings from the element to matrix element.
-}
matrixLookupTable : Dict Char Int
matrixLookupTable =
    List.indexedMap (\i e -> ( e, i )) elements
        |> Dict.fromList


{-| Private: Method to convert a character to it's index within the matrix.
-}
charToIndex : Char -> Maybe Int
charToIndex c =
    Dict.get c matrixLookupTable


{-| Private: Get the number of times this transition is located in the markov graph.
-}
get : Char -> Char -> Markov -> Maybe Int
get from to (Markov matrix _) =
    case ( charToIndex from, charToIndex to ) of
        ( Just fromIndex, Just toIndex ) ->
            Matrix.get fromIndex toIndex matrix
                |> Result.toMaybe

        _ ->
            Nothing


{-| Get the probability of a particular transition happening.
-}
probabilityOf : Char -> Char -> Markov -> Float
probabilityOf from to markov =
    case markov of
        Markov _ 0 ->
            0

        Markov _ count ->
            markov
                |> get from to
                |> Maybe.withDefault 0
                |> (\occurrences -> toFloat occurrences / toFloat count)



-- Modifiers


{-| Add a transition into the markov graph. If the character is not an uppercase or lowercase character or a digit then
the transition is not added.
-}
add : Char -> Char -> Markov -> Markov
add from to markov =
    let
        set value =
            Maybe.map2
                (\fromIndex toIndex ->
                    case markov of
                        Markov matrix count ->
                            Markov (Matrix.set fromIndex toIndex value matrix) (count + 1)
                )
                (charToIndex from)
                (charToIndex to)
                |> Maybe.withDefault markov
    in
    get from to markov
        |> Maybe.map ((+) 1)
        |> Maybe.map set
        |> Maybe.withDefault markov


addList : List String -> Markov -> Markov
addList strings markov =
    strings
        |> List.map String.toList
        |> List.map List.Extra.groupsOfTwo
        |> List.foldl addTransitionList markov


{-| Add a list of transitions.
-}
addTransitionList : List ( Char, Char ) -> Markov -> Markov
addTransitionList trainingData markov =
    trainingData
        |> List.foldl (\( from, to ) -> add from to) markov



-- Generate


word : Markov -> Generator String
word _ =
    Random.constant "fdsa"
