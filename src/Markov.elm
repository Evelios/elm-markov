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

import Array exposing (Array)
import Dict exposing (Dict)
import List.Extra
import List.Util
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
    = Markov
        { matrix : Matrix Int
        , total : Int
        , alphabet : List Char
        , alphabetLookup : Dict Char Int
        }



-- Builders


{-| Create an empty markov chain with no elements in it. This is needed to add further elements into it to start to
train the model.
-}
empty : List Char -> Markov
empty alphabet =
    let
        numElements =
            List.length alphabet
    in
    Markov
        { matrix = Matrix.repeat numElements numElements 0
        , total = 0
        , alphabet = alphabet
        , alphabetLookup = Dict.fromList <| List.indexedMap (\i a -> ( a, i )) alphabet
        }



-- Accessors


{-| Private: Method to convert a character to it's index within the matrix.
-}
charToIndex : Char -> Markov -> Maybe Int
charToIndex c (Markov model) =
    Dict.get c model.alphabetLookup


{-| Private: Get the number of times this transition is located in the markov graph.
-}
get : Char -> Char -> Markov -> Maybe Int
get from to markov =
    case markov of
        Markov model ->
            case ( charToIndex from markov, charToIndex to markov ) of
                ( Just fromIndex, Just toIndex ) ->
                    Matrix.get fromIndex toIndex model.matrix
                        |> Result.toMaybe

                _ ->
                    Nothing


{-| Get the probability of a particular transition happening.
-}
probabilityOf : Char -> Char -> Markov -> Float
probabilityOf from to (Markov markov) =
    case markov.total of
        0 ->
            0

        total ->
            Markov markov
                |> get from to
                |> Maybe.withDefault 0
                |> (\occurrences -> toFloat occurrences / toFloat total)



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
                        Markov model ->
                            Markov
                                { model
                                    | matrix = Matrix.set fromIndex toIndex value model.matrix
                                    , total = model.total + 1
                                }
                )
                (charToIndex from markov)
                (charToIndex to markov)
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
        |> List.map List.Util.groupsOfTwo
        |> List.foldl addTransitionList markov


{-| Add a list of transitions.
-}
addTransitionList : List ( Char, Char ) -> Markov -> Markov
addTransitionList trainingData markov =
    trainingData
        |> List.foldl (\( from, to ) -> add from to) markov



-- Generate


word : Markov -> Generator (List Char)
word (Markov model) =
    let
        maybeGenStartingChar : Maybe (Generator Char)
        maybeGenStartingChar =
            case model.alphabet of
                firstEle :: remainingEle ->
                    Just <| Random.uniform firstEle remainingEle

                [] ->
                    Nothing

        -- Should probably be a maybe
        nextChar : Char -> Generator Char
        nextChar prevChar =
            let
                weights : List Float
                weights =
                    charToIndex prevChar (Markov model)
                        -- Need to fix this
                        |> Maybe.withDefault 0
                        |> (\row -> Matrix.getRow row model.matrix)
                        |> Result.map Array.toList
                        |> Result.withDefault (List.repeat (List.length model.alphabet) 1)
                        |> (\intWeights ->
                                List.map (\weight -> toFloat weight / (toFloat <| List.sum intWeights)) intWeights
                           )

                possibilities : List ( Float, Char )
                possibilities =
                    List.Extra.zip weights model.alphabet
            in
            case possibilities of
                firstPossibility :: remainingPossibilities ->
                    Random.weighted firstPossibility remainingPossibilities

                [] ->
                    Random.constant ' '

        generateWord : Int -> Generator Char -> Generator (List Char) -> Generator (List Char)
        generateWord depth prev acc =
            case depth of
                0 ->
                    acc

                _ ->
                    let
                        next =
                            Random.andThen nextChar prev

                        nextAcc =
                            Random.map2 (\a n -> List.append a [ n ]) acc next
                    in
                    generateWord (depth - 1) next nextAcc
    in
    case maybeGenStartingChar of
        Just genStartingChar ->
            generateWord 5 genStartingChar (Random.map List.singleton genStartingChar)

        Nothing ->
            Random.constant []
