module Intcodes.Intcodes exposing (OpResult(..), continue, run, start, step)

import Array exposing (Array)
import Utils exposing (intsToString)


type Instruction
    = ReadCode
    | Arithmetic (Int -> Int -> Int) ( Mode, Mode )
    | Input
    | Output Mode
    | JumpIf (Int -> Bool) ( Mode, Mode )
    | ChangeBase Mode
    | Halt
    | Unrecognised


type Mode
    = Immediate
    | Position
    | Relative


type alias Memory =
    { ar : Array Int
    , pos : Int
    , base : Int
    , input : List Int
    , output : List Int
    , instruction : Instruction
    , registers : List Int
    }


type OpResult
    = Done { output : List Int, finalState : List Int }
    | Next Memory
    | Waiting Memory
    | Fail String (Maybe Memory)


initMemory : List Int -> List Int -> Memory
initMemory program inputs =
    { ar = Array.fromList program
    , pos = 0
    , base = 0
    , input = inputs
    , output = []
    , instruction = ReadCode
    , registers = []
    }


next : Memory -> OpResult
next mem =
    Next { mem | pos = mem.pos + 1 }


consumeRegisters : Memory -> Memory
consumeRegisters mem =
    { mem | registers = [] }


nextInstruction : Memory -> OpResult
nextInstruction mem =
    next { mem | instruction = ReadCode }


expand : Int -> Array Int -> Array Int
expand toPos ar =
    if toPos < 0 then
        ar

    else
        let
            size =
                Array.length ar
        in
        if size > toPos then
            ar

        else
            Array.append ar <| Array.repeat (toPos - size + 1) 0


step : OpResult -> OpResult
step state =
    case state of
        Next mem ->
            let
                { pos, ar, instruction, registers } =
                    mem
            in
            case instruction of
                Unrecognised ->
                    Fail "Unrecognised Opcode" (Just mem)

                Halt ->
                    Done
                        { output = mem.output
                        , finalState = Array.toList ar
                        }

                ReadCode ->
                    doReadCode mem

                Arithmetic f ( mode1, mode2 ) ->
                    case List.length registers of
                        0 ->
                            getValue mode1 pos mem

                        1 ->
                            getValue mode2 pos mem

                        _ ->
                            doArithmetic f mem

                JumpIf f ( mode1, mode2 ) ->
                    case List.length registers of
                        0 ->
                            getValue mode1 pos mem

                        1 ->
                            getValue mode2 pos mem

                        _ ->
                            jumpIf f mem

                Input ->
                    doInput mem

                Output mode ->
                    case List.length registers of
                        0 ->
                            getValue mode pos mem

                        _ ->
                            doOutput mem

                ChangeBase mode ->
                    case List.length registers of
                        0 ->
                            getValue mode pos mem

                        _ ->
                            changeBase mem

        _ ->
            state


doReadCode : Memory -> OpResult
doReadCode mem =
    case Array.get mem.pos mem.ar of
        Nothing ->
            Fail "Opcode out of bounds" (Just mem)

        Just code ->
            case readOpcode code of
                Unrecognised ->
                    Fail ("Unrecognised code " ++ String.fromInt code) (Just mem)

                instruction ->
                    next { mem | instruction = instruction }


readMode : Int -> Maybe ( Mode, Int )
readMode n =
    let
        nextDigit =
            n // 10
    in
    case modBy 10 n of
        0 ->
            Just ( Position, nextDigit )

        1 ->
            Just ( Immediate, nextDigit )

        2 ->
            Just ( Relative, nextDigit )

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


readOpcode : Int -> Instruction
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

        boolean operator =
            \x y ->
                if operator x y then
                    1

                else
                    0
    in
    case op of
        1 ->
            with2Modes (\m -> Arithmetic (+) m)

        2 ->
            with2Modes (\m -> Arithmetic (*) m)

        3 ->
            Input

        4 ->
            with1Mode (\m -> Output m)

        5 ->
            with2Modes (\m -> JumpIf ((==) 1) m)

        6 ->
            with2Modes (\m -> JumpIf ((==) 0) m)

        7 ->
            with2Modes (\m -> Arithmetic (boolean (<)) m)

        8 ->
            with2Modes (\m -> Arithmetic (boolean (==)) m)

        9 ->
            with1Mode (\m -> ChangeBase m)

        99 ->
            Halt

        _ ->
            Unrecognised


doArithmetic : (Int -> Int -> Int) -> Memory -> OpResult
doArithmetic op mem =
    case mem.registers of
        [] ->
            Fail "Registers are not populated" (Just mem)

        _ :: [] ->
            Fail "Registers are not populated" (Just mem)

        x :: (y :: _) ->
            setValue (op x y) (consumeRegisters mem)


getValue : Mode -> Int -> Memory -> OpResult
getValue mode pos mem =
    let
        expandedAr =
            expand pos mem.ar

        newMem =
            { mem | ar = expandedAr }

        immediateValue =
            Array.get pos expandedAr
    in
    case immediateValue of
        Nothing ->
            Fail "Argument out of bounds" (Just newMem)

        Just value ->
            case mode of
                Immediate ->
                    next { newMem | registers = mem.registers ++ [ value ] }

                Position ->
                    getValue Immediate value newMem

                Relative ->
                    getValue Immediate (mem.base + value) newMem


setValue : Int -> Memory -> OpResult
setValue val mem =
    case Array.get mem.pos mem.ar of
        Nothing ->
            Fail "Argument out of bounds" (Just mem)

        Just i ->
            if i < 0 then
                Fail "Write position is negative" (Just mem)

            else
                nextInstruction { mem | ar = Array.set i val <| expand i mem.ar }


doInput : Memory -> OpResult
doInput mem =
    case mem.input of
        [] ->
            Waiting mem

        x :: xs ->
            setValue x { mem | input = xs }


doOutput : Memory -> OpResult
doOutput mem =
    case List.head mem.registers of
        Nothing ->
            Fail "Registers not populated" (Just mem)

        Just value ->
            Next
                (consumeRegisters
                    { mem
                        | output = mem.output ++ [ value ]
                        , instruction = ReadCode
                    }
                )


jumpIf : (Int -> Bool) -> Memory -> OpResult
jumpIf func mem =
    case mem.registers of
        [] ->
            Fail "Registers not populated" (Just mem)

        _ :: [] ->
            Fail "Registers not populated" (Just mem)

        value :: (jumpPos :: _) ->
            if func value then
                Next { mem | pos = jumpPos, instruction = ReadCode }

            else
                nextInstruction (consumeRegisters mem)


changeBase : Memory -> OpResult
changeBase mem =
    case List.head mem.registers of
        Nothing ->
            Fail "Registers not populated" (Just mem)

        Just newBase ->
            nextInstruction { mem | base = newBase }


run : List Int -> List Int -> OpResult
run program inputs =
    start program inputs |> runThrough


start : List Int -> List Int -> OpResult
start program inputs =
    Next (initMemory program inputs)


runThrough : OpResult -> OpResult
runThrough state =
    case state of
        Next _ ->
            runThrough (step state)

        _ ->
            state


continue : OpResult -> List Int -> OpResult
continue state input =
    case state of
        Waiting mem ->
            runThrough <| doInput { mem | input = input }

        Next _ ->
            runThrough state

        Done { output } ->
            Fail
                ("Already Done, with output " ++ intsToString output)
                Nothing

        Fail _ _ ->
            state
