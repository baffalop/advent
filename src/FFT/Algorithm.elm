module FFT.Algorithm exposing
    ( calculateElement
    , calculateNPhases
    , calculatePhase
    , getMessage
    , getMessageOffset
    , nSimplifiedPhases
    , splitNumber
    )

import Utils


splitNumber : String -> List Int
splitNumber inputString =
    String.split "" inputString
        |> List.map String.toInt
        |> Utils.filterMaybes


sumEvery : Int -> Int -> List Int -> Int
sumEvery takeCount skipCount input =
    case input of
        [] ->
            0

        _ ->
            let
                sum =
                    List.take takeCount input
                        |> List.sum

                rest =
                    List.drop (takeCount + skipCount) input
            in
            sum + sumEvery takeCount skipCount rest


sumPositives : List Int -> Int -> Int
sumPositives input index =
    let
        preparedList =
            List.drop (index - 1) input
    in
    sumEvery index (index * 3) preparedList


sumNegatives : List Int -> Int -> Int
sumNegatives input index =
    let
        skipCount =
            index * 3

        preparedList =
            List.drop (skipCount - 1) input
    in
    sumEvery index skipCount preparedList


lastDigit : Int -> Int
lastDigit =
    abs >> modBy 10


calculateElement : List Int -> Int -> Int
calculateElement input index =
    (sumPositives input index - sumNegatives input index)
        |> lastDigit


calculatePhase : List Int -> List Int
calculatePhase input =
    List.range 1 (List.length input)
        |> List.map (calculateElement input)


calculateNPhases : Int -> List Int -> List Int
calculateNPhases n input =
    if n == 0 then
        input

    else
        calculateNPhases (n - 1) (calculatePhase input)



-- PART 2


getMessageOffset : List Int -> Int
getMessageOffset input =
    List.take 7 input
        |> List.map String.fromInt
        |> List.foldr (++) ""
        |> String.toInt
        |> Maybe.withDefault 0


prepareFullInput : List Int -> List Int
prepareFullInput baseInput =
    let
        inputLength =
            List.length baseInput

        messageOffset =
            getMessageOffset baseInput

        discardedRepetitions =
            messageOffset // inputLength

        firstRepetitionOffset =
            modBy inputLength messageOffset
    in
    List.repeat (10000 - discardedRepetitions) baseInput
        |> List.concat
        |> List.drop firstRepetitionOffset



-- The 2nd half of the phase calculation boils down to this.
-- Since the message offset is in the 2nd half, we need not worry about anything else.


calculateSimplifiedPhase : List Int -> List Int
calculateSimplifiedPhase =
    List.foldr
        (\value totals ->
            (List.head totals
                |> Maybe.withDefault 0
                |> (+) value
                |> lastDigit
            )
                :: totals
        )
        []


nSimplifiedPhases : Int -> List Int -> List Int
nSimplifiedPhases n input =
    if n == 0 then
        input

    else
        nSimplifiedPhases (n - 1) (calculateSimplifiedPhase input)


getMessage : List Int -> Int
getMessage =
    prepareFullInput
        >> nSimplifiedPhases 100
        >> List.take 8
        >> List.foldl (\digit n -> n * 10 + digit) 0
