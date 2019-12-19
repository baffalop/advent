module Intcodes.Intcodes exposing (process)

import Array exposing (Array)


type Opcode
    = Add ( Mode, Mode )
    | Mult ( Mode, Mode )
    | Input
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


read2Modes : Int -> Maybe ( Mode, Mode )
read2Modes n =
    let
        mode1 =
            readMode n

        mode2 =
            Maybe.andThen (Tuple.second >> readMode) mode1
    in
    Maybe.map2 Tuple.pair mode1 mode2
        |> Maybe.map (Tuple.mapBoth Tuple.first Tuple.first)


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
            Input

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
    , inputs : List Int
    , outputs : List Int
    }


initMemory : List Int -> List Int -> Memory
initMemory program inputs =
    { ar = Array.fromList program
    , pos = 0
    , inputs = inputs
    , outputs = []
    }


type OpResult
    = Done { outputs : List Int, finalState : List Int }
    | Fail String Memory


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
                    Done
                        { outputs = mem.outputs
                        , finalState = Array.toList mem.ar
                        }

                Add modes ->
                    doArithmetic (+) modes mem

                Mult modes ->
                    doArithmetic (*) modes mem

                Input ->
                    doInput mem

                Output mode ->
                    doOutput mode mem


doArithmetic : (Int -> Int -> Int) -> ( Mode, Mode ) -> Memory -> OpResult
doArithmetic op ( mode1, mode2 ) mem =
    let
        arg1 =
            getValue mode1 1 mem

        arg2 =
            getValue mode2 2 mem
    in
    case Maybe.map2 op arg1 arg2 of
        Nothing ->
            Fail "Argument out of bounds" mem

        Just result ->
            setValue 3 result mem


setValue : Int -> Int -> Memory -> OpResult
setValue offset val mem =
    let
        pos =
            mem.pos + offset

        checkBeforeSetting =
            \i ->
                Array.get i mem.ar
                    |> Maybe.map
                        (always
                            { mem
                                | ar = Array.set i val mem.ar
                                , pos = pos + 1
                            }
                        )

        writePosition =
            Array.get pos mem.ar
    in
    case writePosition of
        Nothing ->
            Fail "Param out of bounds" mem

        Just i ->
            case checkBeforeSetting i of
                Nothing ->
                    Fail "Write position out of bounds" mem

                Just newMem ->
                    doNextOpcode newMem


doInput : Memory -> OpResult
doInput mem =
    case List.head mem.inputs of
        Nothing ->
            Fail "Expected input" mem

        Just input ->
            setValue 1 input mem


doOutput : Mode -> Memory -> OpResult
doOutput mode mem =
    case getValue mode 1 mem of
        Nothing ->
            Fail "Argument out of bounds" mem

        Just value ->
            doNextOpcode
                { mem
                    | outputs = mem.outputs ++ [ value ]
                    , pos = mem.pos + 2
                }


process : List Int -> List Int -> OpResult
process program inputs =
    initMemory program inputs |> doNextOpcode
