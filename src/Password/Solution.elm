module Password.Solution exposing (explodeInt, implodeInt, passwordsInRange)

import Utils exposing (littleTail)


explodeInt : Int -> List Int
explodeInt n =
    if n < 10 then
        [ n ]

    else
        explodeInt (n // 10) ++ [ modBy 10 n ]


implodeInt : List Int -> Int
implodeInt input =
    List.foldl
        (\digit ( n, len ) ->
            let
                mult =
                    10 ^ (len - 1)
            in
            ( n + (digit * mult), len - 1 )
        )
        ( 0, List.length input )
        input
        |> Tuple.first


whereMonotonicityBreaks : List Int -> Maybe Int
whereMonotonicityBreaks input =
    case input of
        [] ->
            Nothing

        _ :: [] ->
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

        _ :: [] ->
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
