module Intcodes.Solver exposing (permute, tryAll)

import Intcodes.Intcodes exposing (OpResult(..), process)
import Intcodes.Program exposing (program)


run : ( Int, Int ) -> OpResult
run params =
    let
        noun =
            Tuple.first params

        verb =
            Tuple.second params

        firstOp =
            Maybe.withDefault 1 (List.head program)
    in
    process ([ firstOp ] ++ [ noun, verb ] ++ List.drop 3 program)


permute : Int -> List ( Int, Int )
permute limit =
    let
        range =
            List.range 0 limit

        cycle =
            \n -> List.map (Tuple.pair n) range
    in
    List.concatMap cycle range


runAll : List ( Int, Int ) -> List ( ( Int, Int ), OpResult ) -> List ( ( Int, Int ), OpResult )
runAll paramList history =
    case paramList of
        [] ->
            history

        params :: remainingParams ->
            runAll remainingParams (( params, run params ) :: history)


tryAll : Int -> List ( Int, Int ) -> Maybe ( Int, Int )
tryAll match paramList =
    case paramList of
        [] ->
            Nothing

        params :: remainingParams ->
            case run params of
                Done (result :: _) ->
                    if result == match then
                        Just params

                    else
                        tryAll match remainingParams

                _ ->
                    tryAll match remainingParams
