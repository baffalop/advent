module Amps.Amps exposing (optimiseAmps, runAmps)

import Intcodes.Intcodes as Intcodes exposing (OpResult(..))
import Set
import Utils exposing (filterMaybes)


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


permuteUnique : Int -> List (List Int)
permuteUnique limit =
    let
        seed =
            List.repeat limit (List.range 0 (limit - 1))

        addToPermutations add perms =
            List.concatMap
                (\n ->
                    List.filter (not << List.member n) perms
                        |> List.map ((::) n)
                )
                add
    in
    List.foldl addToPermutations (List.repeat limit []) seed
        |> Set.fromList
        |> Set.toList


optimiseAmps : List Int -> Int -> Maybe ( List Int, List Int )
optimiseAmps program count =
    let
        resultsToMaybe ( result, phases ) =
            case result of
                Err _ ->
                    Nothing

                Ok x ->
                    Just ( x, phases )
    in
    List.map (\phases -> ( runAmps program phases, phases )) (permuteUnique count)
        |> List.map resultsToMaybe
        |> filterMaybes
        |> List.sortBy (Tuple.first >> List.head >> Maybe.withDefault 0 >> negate)
        |> List.head
