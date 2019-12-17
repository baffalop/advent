module Password.Solution exposing (..)


explode : Int -> List Int
explode n =
    if n < 10 then
        [ n ]

    else
        explode (n // 10) ++ [ modBy 10 n ]


implode : List Int -> Int
implode input =
    case input of
        [] ->
            0

        n :: ns ->
            let
                mult =
                    10 ^ List.length ns
            in
            (n * mult) + implode ns


bigHead : List a -> List a
bigHead list =
    List.take (List.length list - 1) list


littleTail : List a -> Maybe a
littleTail list =
    List.drop (List.length list - 1) list
        |> List.head


whereMonotonicityBreaks : List Int -> Maybe Int
whereMonotonicityBreaks input =
    case input of
        [] ->
            Nothing

        n :: [] ->
            Nothing

        n :: (m :: ms) ->
            if m < n then
                Just 1

            else
                case whereMonotonicityBreaks (m :: ms) of
                    Nothing ->
                        Nothing

                    Just pos ->
                        Just (pos + 1)


fixMonotonicity : List Int -> List Int
fixMonotonicity input =
    case whereMonotonicityBreaks input of
        Nothing ->
            input

        Just pos ->
            let
                head =
                    List.take pos input

                lastN =
                    List.reverse head |> List.head

                tailLength =
                    List.drop pos input |> List.length
            in
            case lastN of
                Nothing ->
                    input

                Just n ->
                    head ++ List.repeat tailLength n


increment : List Int -> List Int
increment input =
    let
        lsd =
            littleTail input

        rest =
            bigHead input
    in
    case lsd of
        Nothing ->
            [ 1 ]

        Just n ->
            if n + 1 > 9 then
                increment rest ++ [ 0 ]

            else
                rest ++ [ n + 1 ]


hasRepeats : List Int -> Bool
hasRepeats input =
    case input of
        [] ->
            False

        x :: [] ->
            False

        x :: (y :: ys) ->
            if x == y then
                True

            else
                hasRepeats (y :: ys)


greaterThan : List Int -> List Int -> Bool
greaterThan list1 list2 =
    let
        length1 =
            List.length list1

        length2 =
            List.length list2
    in
    if length1 > length2 then
        True

    else if length2 > length1 then
        False

    else
        --todo
        False


monotonicListsInRange : List Int -> List Int -> List (List Int)
monotonicListsInRange lower upper =
    if implode lower > implode upper then
        []

    else
        let
            nextMonotonic =
                fixMonotonicity lower
        in
        nextMonotonic :: monotonicListsInRange (increment nextMonotonic) upper


monotonicRange : Int -> Int -> List Int
monotonicRange lower upper =
    monotonicListsInRange (explode lower) (explode upper)
        |> List.map implode
