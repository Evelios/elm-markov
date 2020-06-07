module List.Extra exposing (groupsOfTwo)


groupsOfTwo : List a -> List ( a, a )
groupsOfTwo list =
    list
        |> List.foldr
            (\a ( acc, maybeLast ) ->
                case maybeLast of
                    Just last ->
                        ( ( a, last ) :: acc, Just a )

                    Nothing ->
                        ( acc, Just a )
            )
            ( [], Nothing )
        |> Tuple.first
