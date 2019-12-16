module Intcodes exposing (Intcodes, OpResult(..), process)

import Array as A


type alias Array a =
    A.Array a


type Opcode
    = Add
    | Mult
    | Halt
    | Unrecognised


type alias Intcodes =
    { ar : Array Int
    , pos : Int
    }


readCode : Int -> Opcode
readCode code =
    case code of
        1 ->
            Add

        2 ->
            Mult

        99 ->
            Halt

        _ ->
            Unrecognised


type OpResult
    = Done (List Int)
    | Next Intcodes
    | Fail String Intcodes


doCode : Intcodes -> OpResult
doCode codes =
    case A.get codes.pos codes.ar of
        Nothing ->
            Fail "Opcode out of bounds" codes

        Just code ->
            case readCode code of
                Unrecognised ->
                    Fail ("Unrecognised code " ++ String.fromInt code) codes

                Halt ->
                    Done (A.toList codes.ar)

                Add ->
                    doOp (+) codes

                Mult ->
                    doOp (*) codes


doOp : (Int -> Int -> Int) -> Intcodes -> OpResult
doOp op codes =
    let
        get =
            \i -> A.get i codes.ar

        firstTarget =
            get (codes.pos + 1)

        secondTarget =
            get (codes.pos + 2)

        first =
            Maybe.andThen get firstTarget

        second =
            Maybe.andThen get secondTarget
    in
    if firstTarget == Nothing || secondTarget == Nothing then
        Fail "Operand target out of bounds" codes

    else
        case Maybe.map2 op first second of
            Nothing ->
                Fail "Operand out of bounds" codes

            Just result ->
                placeResult result codes


placeResult : Int -> Intcodes -> OpResult
placeResult result codes =
    let
        get =
            \i -> A.get i codes.ar
    in
    case get (codes.pos + 3) of
        Nothing ->
            Fail "Place target out of bounds" codes

        Just place ->
            case get place of
                Nothing ->
                    Fail "Place out of bounds" codes

                Just _ ->
                    Next
                        { ar = A.set place result codes.ar
                        , pos = codes.pos + 4
                        }


doAllCodes : Intcodes -> OpResult
doAllCodes codes =
    let
        result =
            doCode codes
    in
    case result of
        Next resultCodes ->
            doAllCodes resultCodes

        otherwise ->
            result


process : List Int -> OpResult
process input =
    doAllCodes
        { ar = A.fromList input
        , pos = 0
        }
