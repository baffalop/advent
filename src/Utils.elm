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


find : comparable -> List comparable -> Maybe Int
find needle list =
    List.foldl
        (\item result ->
            case result of
                Ok i ->
                    Ok i

                Err i ->
                    if item == needle then
                        Ok i

                    else
                        Err (i + 1)
        )
        (Err 0)
        list
        |> (\result ->
                case result of
                    Err _ ->
                        Nothing

                    Ok i ->
                        Just i
           )


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
