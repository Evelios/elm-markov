module Markov exposing
    ( Markov, Element(..)
    , empty
    , alphabet, probability, transitionProbabilities
    , add, train, trainList
    , SequenceSettings, generateSequence
    , encode, decode
    )

{-| A markov model is a representation internally of transition
probabilities. This can be used for many different applications. The easiest to explain are creating random word
generators (see the `Markov.String` module) and random sentence generators. These are created by training the model
with a particular corpus of material of a particular genre. You can then create new


# Types

@docs Markov, Element


# Builders

@docs empty


# Accessors

@docs alphabet, probability, transitionProbabilities


# Modifiers

@docs add, train, trainList


# Generation

@docs SequenceSettings, generateSequence


# Json

@docs encode, decode

-}

import Dict.Any exposing (AnyDict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Util
import Random exposing (Generator)


{-| This is the main data structure for the markov models. This data structure internally uses a sparse representation
for transition probabilities. This will be performant when you have many different possible states and they don't all
connect to each other. If you have a lot of transition states and each state can transition to almost every other state,
this implementation will not be as performant as it could be.
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


{-| Get the alphabet of the current markov model.
-}
alphabet : Markov comparable a -> List a
alphabet (Markov transitionTable _) =
    Dict.Any.keys transitionTable
        |> extractElements


{-| Get the probability of a particular transition state.
-}
probability : Element a -> Element a -> Markov comparable a -> Float
probability from to markov =
    List.foldl
        (\( element, eleProb ) _ ->
            if element == to then
                Just eleProb

            else
                Nothing
        )
        Nothing
        (transitionProbabilities from markov)
        |> Maybe.withDefault 0


{-| For a particular element, get the probabilities for transitioning from the input element to all the other elements
which are accessible from the input elements. This only returns elements with non-zero probabilities. All probabilities
are normalized to be within the (0 -> 1] range.
-}
transitionProbabilities : Element a -> Markov comparable a -> List ( Element a, Float )
transitionProbabilities from (Markov transitionTable _) =
    let
        maybeTransitionCounts =
            Dict.Any.get from transitionTable
                |> Maybe.map Dict.Any.toList
    in
    case maybeTransitionCounts of
        Just transitionCounts ->
            let
                total =
                    List.map Tuple.second transitionCounts
                        |> List.sum
            in
            transitionCounts
                |> List.map (\( e, count ) -> ( e, toFloat count / toFloat total ))

        Nothing ->
            []



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


{-| Add a sequence of transitions. This function adds the Start and End to the list so that you are able to train
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


{-| Train the markov model on multiple sequences at once.
-}
trainList : List (List a) -> Markov comparable a -> Markov comparable a
trainList trainingLists markov =
    List.foldl train markov trainingLists



-- Helper Functions


{-| Private: Extract all of the user objects from a list of markov elements.
-}
extractElements : List (Element a) -> List a
extractElements =
    List.filterMap
        (\e ->
            case e of
                Element a ->
                    Just a

                _ ->
                    Nothing
        )



-- Generate


{-| The parameters used for `generateSequence`. This are setup in record format so that this function can be extended
later.
-}
type alias SequenceSettings =
    { maxLength : Int
    }


{-| Generate a sequence of elements using the probabilities within the markov transition graph. This sequence should
come out to be a random length and takes into account the `Start -> a` and the `a -> End` transition probabilities.
This ensures that the starting and ending weights are taken into account as well.
-}
generateSequence : SequenceSettings -> Markov comparable a -> Generator (List a)
generateSequence settings markov =
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

        genNextElement : Element a -> Generator (Element a)
        genNextElement prevElement =
            let
                probabilities =
                    transitionProbabilities prevElement markov
                        |> List.map (\( a, b ) -> ( b, a ))
            in
            case probabilities of
                firstPossibility :: remainingPossibilities ->
                    Random.weighted firstPossibility remainingPossibilities

                [] ->
                    Random.constant End
    in
    phraseHelper settings.maxLength Start []
        |> Random.map extractElements



-- Json


{-| Encode a markov graph into a json object. In order to encode a markov graph we need to know how to encode your
object into json data. Unfortunately because of the current implementation, the object needs to be converted into a
string so that it can be used as the json "key" object for storing transition probabilities.
-}
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


{-| Decode a json object into a markov graph. In order to decode the model, you need to have the inverse of the encoding
function. This function needs to be able to convert the string object you created into your object. It also needs to
take the same function that you used to create the markov graph in the `Markov.empty` function. This is what was used
to store that element into a transition graph.
-}
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
