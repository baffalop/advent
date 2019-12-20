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
