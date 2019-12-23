module Intcodes.Intcodes exposing (OpResult(..), run)

import Array exposing (Array)


type Opcode
    = Add ( Mode, Mode )
    | Mult ( Mode, Mode )
    | Input
    | Output Mode
    | JumpIfTrue ( Mode, Mode )
    | JumpIfFalse ( Mode, Mode )
    | LessThan ( Mode, Mode )
    | Equals ( Mode, Mode )
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

        with1Mode construct =
            case readMode modesDigits of
                Nothing ->
                    Unrecognised

                Just ( mode, _ ) ->
                    construct mode

        with2Modes construct =
            case read2Modes modesDigits of
                Nothing ->
                    Unrecognised

                Just modes ->
                    construct modes
    in
    case op of
        1 ->
            with2Modes (\m -> Add m)

        2 ->
            with2Modes (\m -> Mult m)

        3 ->
            Input

        4 ->
            with1Mode (\m -> Output m)

        5 ->
            with2Modes (\m -> JumpIfTrue m)

        6 ->
            with2Modes (\m -> JumpIfFalse m)

        7 ->
            with2Modes (\m -> LessThan m)

        8 ->
            with2Modes (\m -> Equals m)

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


doNextOpcode : Memory -> OpResult
doNextOpcode mem =
    case Array.get mem.pos mem.ar of
        Nothing ->
            Fail "Opcode out of bounds" mem

        Just code ->
            case readOpcode code of
                Unrecognised ->
                    Fail ("Unrecognised code " ++ String.fromInt code) mem

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

                JumpIfTrue modes ->
                    jumpIf modes identity mem

                JumpIfFalse modes ->
                    jumpIf modes not mem

                LessThan modes ->
                    doArithmetic
                        (\x y ->
                            if x < y then
                                1

                            else
                                0
                        )
                        modes
                        mem

                Equals modes ->
                    doArithmetic
                        (\x y ->
                            if x == y then
                                1

                            else
                                0
                        )
                        modes
                        mem


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


setValue : Int -> Int -> Memory -> OpResult
setValue offset val mem =
    let
        pos =
            mem.pos + offset

        checkBeforeSetting i =
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
            setValue 1
                input
                { mem
                    | inputs =
                        List.tail mem.inputs
                            |> Maybe.withDefault []
                }


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


jumpIf : ( Mode, Mode ) -> (Bool -> Bool) -> Memory -> OpResult
jumpIf ( mode1, mode2 ) func mem =
    case getValue mode1 1 mem of
        Nothing ->
            Fail "Argument out of bounds" mem

        Just value ->
            if func (value /= 0) then
                case getValue mode2 2 mem of
                    Nothing ->
                        Fail "Argument out of bounds" mem

                    Just v ->
                        doNextOpcode { mem | pos = v }

            else
                doNextOpcode { mem | pos = mem.pos + 3 }


run : List Int -> List Int -> OpResult
run program inputs =
    initMemory program inputs |> doNextOpcode
