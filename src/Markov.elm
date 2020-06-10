module Markov exposing
    ( Markov
    , empty
    , add, addList
    , phrase, PhraseSettings
    , encode, decode
    )

{-| Create a markov transition model of string inputs. This creates


# Types

@docs Markov


# Builders

@docs empty, fromList


# Modifiers

@docs add, addList


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
type Markov
    = Markov (AnyDict Int Element (AnyDict Int Element Int))


type Element
    = Start
    | Element Char
    | End



-- Builders


charComparable : Element -> Int
charComparable element =
    case element of
        Start ->
            -2

        Element c ->
            Char.toCode c

        End ->
            -1


{-| Create an empty markov chain with no elements in it. This is needed to add further elements into it to start to
train the model.
-}
empty : Markov
empty =
    Markov <| Dict.Any.empty charComparable



-- Accessors


{-| Private: Get the number of times this transition is located in the markov graph.
-}
get : Element -> Element -> Markov -> Maybe Int
get from to (Markov transitionTable) =
    transitionTable
        |> Dict.Any.get from
        |> Maybe.andThen (Dict.Any.get to)



-- Modifiers


set : Element -> Element -> Int -> Markov -> Markov
set from to value (Markov transitionTable) =
    Dict.Any.get from transitionTable
        |> Maybe.withDefault (Dict.Any.empty charComparable)
        |> Dict.Any.insert to value
        |> (\toTable -> Dict.Any.insert from toTable transitionTable)
        |> Markov


{-| Add a transition into the markov graph. If the character is not an uppercase or lowercase character or a digit then
the transition is not added.
-}
add : Element -> Element -> Markov -> Markov
add from to markov =
    set
        from
        to
        (get from to markov |> Maybe.withDefault 0 |> (+) 1)
        markov


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


type alias PhraseSettings =
    { maxLength : Int
    }


phrase : PhraseSettings -> Markov -> Generator (List Char)
phrase settings markov =
    let
        phraseHelper : Int -> Element -> List Element -> Generator (List Element)
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

        transitionProbabilities : Element -> Markov -> List ( Float, Element )
        transitionProbabilities from (Markov transitionTable) =
            Dict.Any.get from transitionTable
                |> Maybe.map Dict.Any.toList
                |> Maybe.withDefault [ ( End, 1 ) ]
                |> List.map (\( a, b ) -> ( toFloat b, a ))

        genNextElement : Element -> Generator Element
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


encode : Markov -> Value
encode (Markov transitionTable) =
    let
        elementToString element =
            case element of
                Start ->
                    "start"

                End ->
                    "end"

                Element c ->
                    String.fromChar c

        encodeRow row =
            Dict.Any.encode elementToString Encode.int row
    in
    Dict.Any.encode elementToString encodeRow transitionTable


decode : Decoder Markov
decode =
    let
        stringToElement value =
            case value of
                "start" ->
                    Start

                "end" ->
                    End

                _ ->
                    value
                        |> String.toList
                        |> List.head
                        |> Maybe.withDefault ' '
                        |> Element

        decodeDict =
            Dict.Any.decode (\s _ -> stringToElement s) charComparable
    in
    decodeDict (decodeDict Decode.int)
        |> Decode.map Markov
