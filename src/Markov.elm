module Markov exposing
    ( Markov
    , add
    , empty
    , probabilityOf
    )

import Matrix exposing (Matrix)


{-| Create a markov transition model of string inputs. This creates


# Types

@docs Markov


# Builders

@docs fromList


# Modifiers

@docs add


# Generation

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


{-| Private: Method to convert a character to it's index within the matrix.
-}
charToIndex : Char -> Maybe Int
charToIndex c =
    if Char.isUpper c then
        charToIndex <| Char.toLower c

    else if Char.isLower c then
        Just <| Char.toCode c - Char.toCode 'a'

    else if Char.isDigit c then
        Just <| Char.toCode c - Char.toCode '0'

    else
        Nothing


{-| Private: Get the number of times this transition is located in the markov graph.
-}
get : Char -> Char -> Markov -> Maybe Int
get from to (Markov matrix _) =
    let
        maybeFromIndex =
            charToIndex from

        maybeToIndex =
            charToIndex to
    in
    case ( maybeFromIndex, maybeToIndex ) of
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
