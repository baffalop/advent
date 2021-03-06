module Amps.Amps exposing (initAmps, optimiseAmps, permuteUnique, runFeedbackAmps)

import Intcodes.Intcodes as IC exposing (OpResult(..))
import Set
import Utils exposing (flip)


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
    List.map (List.singleton >> IC.run program) phases


runAmpCycle : Int -> List OpResult -> ( Int, List OpResult )
runAmpCycle input amps =
    let
        next result rest =
            case IC.consumeOutput result of
                ( [], _ ) ->
                    ( 0, [ Fail "Last amp did not output" Nothing ] )

                ( output :: [], consumedResult ) ->
                    Tuple.mapSecond ((::) consumedResult) (runAmpCycle output rest)

                ( _, _ ) ->
                    ( 0, [ Fail "Last amp gave too many outputs" Nothing ] )
    in
    case amps of
        [] ->
            ( input, [] )

        amp :: rest ->
            let
                result =
                    IC.continue amp [ input ]
            in
            case result of
                Fail _ _ ->
                    ( input, [ result ] )

                Next mem ->
                    ( 0, [ Fail "Got ourselves into a Next state somehow" (Just mem) ] )

                Waiting _ ->
                    next result rest

                Done _ ->
                    next result rest


allWaiting : List OpResult -> Bool
allWaiting =
    List.foldl
        (\result prev ->
            if not prev then
                False

            else
                case result of
                    Waiting _ ->
                        True

                    _ ->
                        False
        )
        True


runFeedbackAmps : Int -> List OpResult -> ( Int, List OpResult )
runFeedbackAmps input amps =
    let
        ( output, resultStates ) =
            runAmpCycle input amps
    in
    if allWaiting resultStates then
        runFeedbackAmps output resultStates

    else
        ( output, resultStates )


optimiseAmps : List Int -> Int -> Int -> Maybe ( Int, List Int )
optimiseAmps program lower upper =
    let
        permutations =
            permuteUnique lower upper
    in
    List.map (initAmps program >> runFeedbackAmps 0) permutations
        |> List.map Tuple.first
        |> List.map2 (flip Tuple.pair) permutations
        |> List.sortBy (Tuple.first >> negate)
        |> List.head
