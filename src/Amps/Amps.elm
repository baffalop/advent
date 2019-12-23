module Amps.Amps exposing (runAmps)

import Intcodes.Intcodes as Intcodes exposing (OpResult(..))


toResult : OpResult -> Result String (List Int)
toResult opResult =
    case opResult of
        Done { outputs } ->
            Ok outputs

        Fail message _ ->
            Err message


runAmps : List Int -> List Int -> Result String (List Int)
runAmps program phases =
    let
        runAmp : Int -> Result String (List Int) -> Result String (List Int)
        runAmp phase lastResult =
            Result.andThen
                (\input -> (phase :: input) |> Intcodes.run program |> toResult)
                lastResult
    in
    List.foldl runAmp (Ok [ 0 ]) phases
