module Amps.Amps exposing (optimiseAmps, permuteUnique, runAmps)

import Intcodes.Intcodes as Intcodes exposing (OpResult(..))
import Set
import Utils exposing (filterMaybes, flip)


permuteUnique : Int -> Int -> List (List Int)
permuteUnique lower upper =
    let
        range =
            List.range lower upper

        seed =
            List.repeat (List.length range) range

        addToPermutations add perms =
            List.concatMap
                (\n ->
                    List.filter (not << List.member n) perms
                        |> List.map ((::) n)
                )
                add
    in
    List.foldl addToPermutations (List.repeat (List.length range) []) seed
        |> Set.fromList
        |> Set.toList


initAmps : List Int -> List Int -> List OpResult
initAmps program phases =
    List.map (List.singleton >> Intcodes.run program) phases


runAmps : List Int -> List OpResult -> ( List Int, List OpResult )
runAmps input amps =
    case amps of
        [] ->
            ( input, [] )

        amp :: rest ->
            let
                result =
                    Intcodes.continue amp input
            in
            case result of
                Fail _ _ ->
                    ( input, [ result ] )

                Waiting { output } ->
                    Tuple.mapSecond ((::) result) (runAmps output rest)

                Done { output } ->
                    Tuple.mapSecond ((::) result) (runAmps output rest)


optimiseAmps : List Int -> Int -> Int -> Maybe ( List Int, List Int )
optimiseAmps program lower upper =
    let
        permutations =
            permuteUnique lower upper
    in
    List.map (initAmps program >> runAmps [ 0 ]) permutations
        |> List.map Tuple.first
        |> List.map2 (flip Tuple.pair) permutations
        |> List.sortBy (Tuple.first >> List.head >> Maybe.withDefault 0 >> negate)
        |> List.head
