module Markov exposing
    ( Markov
    , add, addList
    , word
    , empty
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
import Dict.Any exposing (AnyDict)
import List.Extra
import List.Util
import Matrix exposing (Matrix)
import Random exposing (Generator)



{- A markov graph which represents the chance of transitioning from one element to another. The row is the 'from'
   element and the column is the 'to' element. Each row represents the transition counts for each of those elements.

   Note: Using this matrix library indices are given in (column, row) as per mathematical notation with (0, 0) on the
         top left.

   Example:

       a  b  c
     a 1  2  1
     b 3  2  5
     c 0  1  0

     a -> a = 1/4
     a -> b = 1/2
     a -> c = 1/4
-}


type Markov
    = Markov
        { matrix : Matrix Int
        , alphabet : List Element
        , alphabetLookup : AnyDict Int Element Int
        }


type Element
    = Start
    | Element Char
    | End


elementComparable : Element -> Int
elementComparable e =
    case e of
        Start ->
            -2

        End ->
            -1

        Element char ->
            Char.toCode char



-- Builders


{-| Create an empty markov chain with no elements in it. This is needed to add further elements into it to start to
train the model.
-}
empty : List Char -> Markov
empty alphabet =
    let
        numElements =
            List.length alphabet

        alphabetWithTerminals =
            alphabet
                |> List.map Element
                |> (::) End
                |> (::) Start
    in
    Markov
        { matrix = Matrix.repeat numElements numElements 0
        , alphabet = alphabetWithTerminals
        , alphabetLookup =
            alphabetWithTerminals
                |> List.indexedMap (\i a -> ( a, i ))
                |> Dict.Any.fromList elementComparable
        }



-- Accessors


{-| Private: Method to convert a character to it's index within the matrix.
-}
charToIndex : Element -> Markov -> Maybe Int
charToIndex element (Markov model) =
    Dict.Any.get element model.alphabetLookup


{-| Private: Get the number of times this transition is located in the markov graph.
-}
get : Element -> Element -> Markov -> Maybe Int
get from to markov =
    case markov of
        Markov model ->
            case ( charToIndex from markov, charToIndex to markov ) of
                ( Just fromIndex, Just toIndex ) ->
                    Matrix.get toIndex fromIndex model.matrix
                        |> Result.toMaybe

                _ ->
                    Nothing


{-| Private: Get all the possible transitioning characters from this element. If an element doesn't have a transition property,
then it will default to the terminal node. The list is not normalized.
-}
transitionProbabilities : Element -> Markov -> List ( Float, Element )
transitionProbabilities from (Markov model) =
    charToIndex from (Markov model)
        |> Maybe.andThen (\row -> Result.toMaybe <| Matrix.getRow row model.matrix)
        |> Maybe.map Array.toList
        |> Maybe.map (List.map toFloat)
        |> Maybe.map (\row -> List.Extra.zip row model.alphabet)
        |> Maybe.withDefault [ ( 1, End ) ]



-- Modifiers


{-| Add a transition into the markov graph. If the character is not an uppercase or lowercase character or a digit then
the transition is not added.
-}
add : Element -> Element -> Markov -> Markov
add from to markov =
    let
        set value =
            Maybe.map2
                (\fromIndex toIndex ->
                    case markov of
                        Markov model ->
                            Markov
                                { model
                                    | matrix = Matrix.set toIndex fromIndex value model.matrix
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
        |> List.map (List.map Element)
        |> List.foldl addTransitionList markov


{-| Add a list of transitions.
-}
addTransitionList : List Element -> Markov -> Markov
addTransitionList trainingData markov =
    if List.isEmpty trainingData then
        markov

    else
        trainingData
            |> (::) Start
            |> (\beginning -> List.append beginning [ End ])
            |> List.Util.groupsOfTwo
            |> List.foldl (\( from, to ) -> add from to) markov



-- Generate


word : Markov -> Generator (List Char)
word markov =
    let
        startChar : Generator Element
        startChar =
            nextChar Start

        nextChar : Element -> Generator Element
        nextChar prevElement =
            case transitionProbabilities prevElement markov of
                firstPossibility :: remainingPossibilities ->
                    Random.weighted firstPossibility remainingPossibilities

                [] ->
                    Random.constant End

        generateWord : Int -> Generator Element -> Generator (List Element) -> Generator (List Element)
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

        cleanResults =
            List.filterMap
                (\e ->
                    case e of
                        Start ->
                            Nothing

                        Element a ->
                            Just a

                        End ->
                            Nothing
                )
    in
    generateWord 5 startChar (Random.map List.singleton startChar)
        |> Random.map cleanResults
