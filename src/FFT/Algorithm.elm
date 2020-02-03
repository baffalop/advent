module FFT.Algorithm exposing (calculateNPhases, calculatePhase, splitNumber)

import Utils


basePattern =
    [ 0, 1, 0, -1 ]


splitNumber : String -> List Int
splitNumber inputString =
    String.split "" inputString
        |> List.map String.toInt
        |> Utils.filterMaybes


repeatEach : Int -> List Int -> List Int
repeatEach count =
    List.concatMap (List.repeat count)


repeatToMatch : Int -> List Int -> List Int
repeatToMatch count input =
    let
        targetLength =
            ceiling (toFloat count / toFloat (List.length input))
    in
    List.repeat targetLength input
        |> List.concat
        |> List.take (count + 1)
        |> List.tail
        |> Maybe.withDefault []


lastDigit : Int -> Int
lastDigit =
    abs >> modBy 10


applyPattern : List Int -> List Int -> List Int
applyPattern input pattern =
    let
        repeatedPattern =
            repeatToMatch (List.length input) pattern
    in
    List.map2 (*) repeatedPattern input


calculateElement : List Int -> Int -> Int
calculateElement input index =
    repeatEach index basePattern
        |> applyPattern input
        |> List.sum
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
