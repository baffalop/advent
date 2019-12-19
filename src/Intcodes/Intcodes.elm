module Intcodes.Intcodes exposing (Intcodes, OpResult(..), process)

import Array exposing (Array)


type Opcode
    = Add ( Mode, Mode )
    | Mult ( Mode, Mode )
    | Input Mode
    | Output Mode
    | Halt
    | Unrecognised


type Mode
    = Position
    | Immediate


readMode : Int -> Maybe ( Mode, Int )
readMode n =
    let
        next =
            n // 10
    in
    case modBy 10 n of
        0 ->
            Just ( Position, next )

        1 ->
            Just ( Immediate, next )

        _ ->
            Nothing


read2Modes : Int -> Maybe ( Int, Int )
read2Modes n =
    let
        ( first, remainder ) =
            readMode n

        ( second, _ ) =
            Maybe.andThen readMode remainder
    in
    Maybe.map2 Tuple.pair first second


readOpcode : Int -> Opcode
readOpcode code =
    let
        op =
            modBy 100 code

        modesDigits =
            code // 100
    in
    case op of
        1 ->
            case read2Modes modesDigits of
                Nothing ->
                    Unrecognised

                Just modes ->
                    Add modes

        2 ->
            case read2Modes modesDigits of
                Nothing ->
                    Unrecognised

                Just modes ->
                    Mult modes

        3 ->
            case readMode modesDigits of
                Nothing ->
                    Unrecognised

                Just ( mode, _ ) ->
                    Input mode

        4 ->
            case readMode modesDigits of
                Nothing ->
                    Unrecognised

                Just ( mode, _ ) ->
                    Output mode

        99 ->
            Halt

        _ ->
            Unrecognised


type alias Memory =
    { ar : Array Int
    , pos : Int
    , executing : Maybe Opcode
    , param : Int
    , registers : List Int
    }


initMemory : List Int -> Memory
initMemory input =
    { ar = Array.fromList input
    , pos = 0
    }


type OpResult
    = Done (List Int)
    | Fail String Memory


doNextOpcode : Memory -> OpResult
doNextOpcode mem =
    case Array.get mem.pos mem.ar of
        Nothing ->
            Fail "Opcode out of bounds" mem

        Just code ->
            case readOpcode code of
                Unrecognised ->
                    Fail "Unrecognised code" mem

                Halt ->
                    Done (Array.toList mem.ar)

                Add modes ->
                    doArithmetic (+) modes mem

                Mult modes ->
                    doArithmetic (*) modes mem


getValue : Mode -> Int -> Memory -> Maybe Int
getValue mode offset { ar, pos } =
    let
        immediateValue =
            Array.get (pos + offset) ar
    in
    case mode of
        Immediate ->
            immediateValue

        Position ->
            Maybe.andThen (\i -> Array.get i ar) immediateValue


setValue : Int -> Int -> Memory -> Maybe (Array Int)
setValue offset val { ar, pos } =
    let
        checkBeforeSetting =
            \i ->
                Array.get i ar
                    |> Maybe.andThen
                        (always <|
                            { ar = Array.set i val ar
                            , pos = pos + offset + 1
                            }
                        )
    in
    Array.get (pos + offset) ar
        |> Maybe.andThen checkBeforeSetting


doArithmetic : (Int -> Int -> Int) -> ( Mode, Mode, Mode ) -> Memory -> OpResult
doArithmetic op ( mode1, mode2 ) mem =
    let
        arg1 =
            getValue mode1 1 mem

        arg2 =
            getValue mode2 2 mem
    in
    case Maybe.map2 op arg1 arg2 of
        Nothing ->
            Fail "Operand out of bounds" mem

        Just result ->
            case setValue 3 mem of
                Nothing ->
                    Fail "Output out of bounds" mem

                Just newArray ->
                    doNextOpcode
                        { ar = newArray
                        , pos = mem.pos + 4
                        }


process : List Int -> OpResult
process =
    initMemory |> doNextOpcode
