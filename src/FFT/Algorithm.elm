module FFT.Algorithm exposing (calculateElement, calculateNPhases, calculatePhase, splitNumber)

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
