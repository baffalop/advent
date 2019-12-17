module Password.Solution exposing (explodeInt, implodeInt, passwordsInRange)


explodeInt : Int -> List Int
explodeInt n =
    if n < 10 then
        [ n ]

    else
        explodeInt (n // 10) ++ [ modBy 10 n ]


implodeInt : List Int -> Int
implodeInt input =
    case input of
        [] ->
            0

        n :: ns ->
            let
                mult =
                    10 ^ List.length ns
            in
            (n * mult) + implodeInt ns


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
                    littleTail head

                tailLength =
                    List.length input - pos
            in
            case lastN of
                Nothing ->
                    input

                Just n ->
                    head ++ List.repeat tailLength n


countFirstDigit : List Int -> Int
countFirstDigit input =
    case input of
        [] ->
            0

        x :: [] ->
            1

        x :: (y :: ys) ->
            if x /= y then
                1

            else
                1 + countFirstDigit (y :: ys)


hasRepeats : List Int -> Bool
hasRepeats input =
    case countFirstDigit input of
        0 ->
            False

        2 ->
            True

        n ->
            hasRepeats (List.drop n input)


passwordsInRange : Int -> Int -> List Int
passwordsInRange lower upper =
    let
        asList =
            explodeInt lower

        nextMonotonic =
            fixMonotonicity asList

        nextInt =
            if asList == nextMonotonic then
                lower

            else
                implodeInt nextMonotonic
    in
    if nextInt > upper then
        []

    else if hasRepeats nextMonotonic then
        nextInt :: passwordsInRange (nextInt + 1) upper

    else
        passwordsInRange (nextInt + 1) upper
