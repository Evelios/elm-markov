module Markov exposing
    ( Markov, Element(..)
    , empty
    , add, train
    , phrase, PhraseSettings
    , encode, decode
    )

{-| Create a markov transition model.

Much of this documentation will use examples from the `String` implementation of this module to give you insight into
how to implement this model with the data that you are trying to work with.


# Types

@docs Markov, Element


# Builders

@docs empty, fromList


# Modifiers

@docs add, train


# Generation

@docs phrase, PhraseSettings


# Encoding and Decoding

@docs encode, decode

-}

import Dict.Any exposing (AnyDict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Util
import Random exposing (Generator)


{-| A markov graph is a representation of transition probabilities.
-}
type Markov comparable a
    = Markov (AnyDict comparable (Element a) (AnyDict comparable (Element a) Int)) (Element a -> comparable)


{-| Elements are needed to define the beginning and end of a chain. We don't only need to know the transition
probabilities from one element to another. We need to know the chance that a chain starts with an element, or the chance
that the chain will end with an element.
-}
type Element a
    = Start
    | Element a
    | End



-- Builders


{-| Create an empty markov chain with no elements in it. This is needed to add further elements into it to start to
train the model. In order to store the transition properties, we need a way of serializing your data.

    charComparable : Element Char -> Int
    charComparable element =
        case element of
            Start ->
                -2

            Element c ->
                Char.toCode c

            End ->
                -1

-}
empty : (Element a -> comparable) -> Markov comparable a
empty conversion =
    Markov (Dict.Any.empty conversion) conversion



-- Accessors


{-| Private: Get the number of times this transition is located in the markov graph.
-}
get : Element a -> Element a -> Markov comparable a -> Maybe Int
get from to (Markov transitionTable _) =
    transitionTable
        |> Dict.Any.get from
        |> Maybe.andThen (Dict.Any.get to)



-- Modifiers


{-| Add a transition into the markov graph. If the character is not an uppercase or lowercase character or a digit then
the transition is not added.
-}
add : Element a -> Element a -> Markov comparable a -> Markov comparable a
add from to markov =
    case markov of
        Markov transitionTable conversion ->
            let
                set : Int -> Markov comparable a
                set value =
                    Dict.Any.get from transitionTable
                        |> Maybe.withDefault (Dict.Any.empty conversion)
                        |> Dict.Any.insert to value
                        |> (\toTable -> Dict.Any.insert from toTable transitionTable)
                        |> (\table -> Markov table conversion)
            in
            set (get from to markov |> Maybe.withDefault 0 |> (+) 1)


{-| Add a single string of transitions. This function adds the Start and End to the list so that you are able to train
the data with starting and ending probabilities as well.
-}
train : List a -> Markov comparable a -> Markov comparable a
train trainingData markov =
    if List.isEmpty trainingData then
        markov

    else
        trainingData
            |> List.map Element
            |> (::) Start
            |> (\beginning -> List.append beginning [ End ])
            |> List.Util.groupsOfTwo
            |> List.foldl (\( from, to ) -> add from to) markov


trainList : List (List a) -> Markov comparable a -> Markov comparable a
trainList trainingLists markov =
    List.foldl train markov trainingLists



-- Generate


type alias PhraseSettings =
    { maxLength : Int
    }


phrase : PhraseSettings -> Markov comparable a -> Generator (List a)
phrase settings markov =
    let
        phraseHelper : Int -> Element a -> List (Element a) -> Generator (List (Element a))
        phraseHelper remainingDepth prevElement accumulator =
            case remainingDepth of
                0 ->
                    Random.constant accumulator

                _ ->
                    Random.andThen
                        (\nextElement ->
                            case nextElement of
                                Element _ ->
                                    phraseHelper
                                        (remainingDepth - 1)
                                        nextElement
                                        (List.append accumulator [ nextElement ])

                                _ ->
                                    Random.constant accumulator
                        )
                        (genNextElement prevElement)

        transitionProbabilities : Element a -> Markov comparable a -> List ( Float, Element a )
        transitionProbabilities from (Markov transitionTable _) =
            Dict.Any.get from transitionTable
                |> Maybe.map Dict.Any.toList
                |> Maybe.withDefault [ ( End, 1 ) ]
                |> List.map (\( a, b ) -> ( toFloat b, a ))

        genNextElement : Element a -> Generator (Element a)
        genNextElement prevElement =
            case transitionProbabilities prevElement markov of
                firstPossibility :: remainingPossibilities ->
                    Random.weighted firstPossibility remainingPossibilities

                [] ->
                    Random.constant End

        cleanResults =
            List.filterMap
                (\e ->
                    case e of
                        Element a ->
                            Just a

                        _ ->
                            Nothing
                )
    in
    phraseHelper settings.maxLength Start []
        |> Random.map cleanResults



-- Encoding / Decoding


encode : (a -> String) -> Markov comparable a -> Value
encode toString (Markov transitionTable _) =
    let
        elementToString element =
            case element of
                Start ->
                    "start"

                End ->
                    "end"

                Element value ->
                    toString value

        encodeRow row =
            Dict.Any.encode elementToString Encode.int row
    in
    Dict.Any.encode elementToString encodeRow transitionTable


decode : (String -> a) -> (Element a -> comparable) -> Decoder (Markov comparable a)
decode decoder conversion =
    let
        stringToElement value =
            case value of
                "start" ->
                    Start

                "end" ->
                    End

                _ ->
                    Element <| decoder value

        decodeDict =
            Dict.Any.decode (\s _ -> stringToElement s) conversion
    in
    decodeDict (decodeDict Decode.int)
        |> Decode.map (\table -> Markov table conversion)
