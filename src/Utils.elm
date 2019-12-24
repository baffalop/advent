module Utils exposing (..)


bigHead : List a -> List a
bigHead list =
    List.take (List.length list - 1) list


littleTail : List a -> Maybe a
littleTail list =
    List.drop (List.length list - 1) list
        |> List.head


filterMaybes : List (Maybe a) -> List a
filterMaybes list =
    case list of
        [] ->
            []

        x :: xs ->
            case x of
                Nothing ->
                    filterMaybes xs

                Just a ->
                    a :: filterMaybes xs


flip : (a -> b -> c) -> (b -> a -> c)
flip func =
    \a b -> func b a


curry : (a -> b -> c) -> (( a, b ) -> c)
curry func =
    \( a, b ) -> func a b


uncurry : (( a, b ) -> c) -> (a -> b -> c)
uncurry func =
    \a b -> func ( a, b )


find : comparable -> List comparable -> Maybe Int
find needle =
    (flip List.foldl (Err 0) <|
        \item result ->
            case result of
                Ok i ->
                    Ok i

                Err i ->
                    if item == needle then
                        Ok i

                    else
                        Err (i + 1)
    )
        >> Result.toMaybe


dropTo : comparable -> List comparable -> List comparable
dropTo needle list =
    find needle list
        |> Maybe.map ((+) 1)
        |> Maybe.withDefault (List.length list)
        |> flip List.drop list


takeTo : comparable -> List comparable -> List comparable
takeTo needle list =
    find needle list
        |> Maybe.map ((+) 1)
        |> Maybe.withDefault 0
        |> flip List.take list


intsToString : List Int -> String
intsToString =
    List.foldl ((String.fromInt >> flip (++) ", ") >> flip (++)) ""
