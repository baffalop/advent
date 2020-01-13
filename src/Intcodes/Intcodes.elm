module Intcodes.Intcodes exposing (Instruction(..), Memory, OpResult(..), consumeOutput, continue, run, start, step)

import Array exposing (Array)
import BigInt exposing (BigInt, add, lt, mul)
import Utils exposing (big, eq, flip, intsToString, toInt)


type Instruction
    = ReadCode
    | Arithmetic (BigInt -> BigInt -> BigInt) ( Mode, Mode, Mode )
    | Input Mode
    | Output Mode
    | JumpIf (BigInt -> Bool) ( Mode, Mode )
    | ChangeBase Mode
    | Halt
    | Unrecognised


type Mode
    = Immediate
    | Position
    | Relative


type alias Memory =
    { ar : Array BigInt
    , pos : Int
    , base : Int
    , input : List Int
    , output : List Int
    , instruction : Instruction
    , registers : Registers

    -- for debugging
    , immediateValue : Maybe BigInt
    , positionalValue : Maybe BigInt
    , relativeValue : Maybe BigInt
    }


type Registers
    = Empty
    | One BigInt
    | Both BigInt BigInt
    | Overflow


type OpResult
    = Done { output : List Int, finalState : List BigInt }
    | Next Memory
    | Waiting Memory
    | Fail String (Maybe Memory)



-- MEMORY


initMemory : List Int -> List Int -> Memory
initMemory program inputs =
    { ar = Array.fromList <| List.map BigInt.fromInt program
    , pos = 0
    , base = 0
    , input = inputs
    , output = []
    , instruction = ReadCode
    , registers = Empty
    , immediateValue = List.head program |> Maybe.map BigInt.fromInt
    , positionalValue = Nothing
    , relativeValue = Nothing
    }


next : Memory -> OpResult
next mem =
    let
        value =
            Array.get (mem.pos + 1) mem.ar

        intValue =
            Maybe.andThen toInt value
    in
    Next
        { mem
            | pos = mem.pos + 1
            , immediateValue = value
            , positionalValue = Maybe.andThen (flip Array.get mem.ar) intValue
            , relativeValue = Maybe.andThen ((+) mem.base >> flip Array.get mem.ar) intValue
        }


consumeRegisters : Memory -> Memory
consumeRegisters mem =
    { mem | registers = Empty }


pushToRegisters : BigInt -> Registers -> Registers
pushToRegisters value registers =
    case registers of
        Empty ->
            One value

        One x ->
            Both x value

        Both _ _ ->
            Overflow

        Overflow ->
            Overflow


nextInstruction : Memory -> Memory
nextInstruction mem =
    { mem | instruction = ReadCode }


expand : Int -> Array BigInt -> Array BigInt
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
            Array.append ar <| Array.repeat (toPos - size + 1) (big 0)



-- COMPUTER LOGIC


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

                Arithmetic f ( mode1, mode2, mode3 ) ->
                    case registers of
                        Empty ->
                            getValue mode1 pos mem

                        One _ ->
                            getValue mode2 pos mem

                        Both x y ->
                            setValue mode3 (f x y) (consumeRegisters mem)

                        Overflow ->
                            Fail "Register overflow for arithmetic op" (Just mem)

                JumpIf f ( mode1, mode2 ) ->
                    case registers of
                        Empty ->
                            getValue mode1 pos mem

                        One _ ->
                            getValue mode2 pos mem

                        Both x y ->
                            jumpIf f x y mem

                        Overflow ->
                            Fail "Register overflow for jump-if op" (Just mem)

                Input mode ->
                    doInput mode mem

                Output mode ->
                    case registers of
                        Empty ->
                            getValue mode pos mem

                        One x ->
                            doOutput x mem

                        _ ->
                            Fail "Register overflow for output op" (Just mem)

                ChangeBase mode ->
                    case registers of
                        Empty ->
                            getValue mode pos mem

                        One x ->
                            changeBase x mem

                        _ ->
                            Fail "Register overflow for change base op" (Just mem)

        _ ->
            state


doReadCode : Memory -> OpResult
doReadCode mem =
    case Array.get mem.pos mem.ar of
        Nothing ->
            Fail "Opcode out of bounds" (Just mem)

        Just code ->
            case toInt code of
                Nothing ->
                    Fail ("Code is to large: " ++ BigInt.toString code) (Just mem)

                Just intCode ->
                    case readOpcode intCode of
                        Unrecognised ->
                            Fail ("Unrecognised code " ++ BigInt.toString code) (Just mem)

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


read3Modes : Int -> Maybe ( Mode, Mode, Mode )
read3Modes n =
    let
        mode1 =
            readMode n

        mode2 =
            Maybe.andThen (Tuple.second >> readMode) mode1

        mode3 =
            Maybe.andThen (Tuple.second >> readMode) mode2
    in
    Maybe.map3 (\( x, _ ) ( y, _ ) ( z, _ ) -> ( x, y, z )) mode1 mode2 mode3


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

        with3Modes construct =
            case read3Modes modesDigits of
                Nothing ->
                    Unrecognised

                Just modes ->
                    construct modes

        boolean operator =
            \x y ->
                if operator x y then
                    big 1

                else
                    big 0
    in
    case op of
        1 ->
            with3Modes (\m -> Arithmetic add m)

        2 ->
            with3Modes (\m -> Arithmetic mul m)

        3 ->
            with1Mode (\m -> Input m)

        4 ->
            with1Mode (\m -> Output m)

        5 ->
            with2Modes (\m -> JumpIf (eq (big 0) >> not) m)

        6 ->
            with2Modes (\m -> JumpIf (eq (big 0)) m)

        7 ->
            with3Modes (\m -> Arithmetic (boolean lt) m)

        8 ->
            with3Modes (\m -> Arithmetic (boolean eq) m)

        9 ->
            with1Mode (\m -> ChangeBase m)

        99 ->
            Halt

        _ ->
            Unrecognised


getValue : Mode -> Int -> Memory -> OpResult
getValue mode pos mem =
    let
        expandedAr =
            expand pos mem.ar

        newMem =
            { mem | ar = expandedAr }

        immediateValue =
            Array.get pos expandedAr

        intMap func val =
            case toInt val of
                Nothing ->
                    Fail "Positional value too big" (Just mem)

                Just nextPos ->
                    func nextPos
    in
    case immediateValue of
        Nothing ->
            Fail "Argument out of bounds" (Just newMem)

        Just value ->
            case mode of
                Immediate ->
                    next { newMem | registers = pushToRegisters value mem.registers }

                Position ->
                    intMap (\v -> getValue Immediate v newMem) value

                Relative ->
                    intMap (\v -> getValue Immediate (mem.base + v) newMem) value


setValue : Mode -> BigInt -> Memory -> OpResult
setValue mode val mem =
    case Array.get mem.pos mem.ar of
        Nothing ->
            Fail "Argument out of bounds" (Just mem)

        Just i ->
            let
                target =
                    case mode of
                        Immediate ->
                            big mem.pos

                        Position ->
                            i

                        Relative ->
                            add (big mem.base) i
            in
            case toInt target of
                Nothing ->
                    Fail "Write position is too big" (Just mem)

                Just writePos ->
                    if writePos < 0 then
                        Fail "Write position is negative" (Just mem)

                    else
                        next <|
                            nextInstruction
                                { mem
                                    | ar =
                                        Array.set writePos val <|
                                            expand writePos mem.ar
                                }


doInput : Mode -> Memory -> OpResult
doInput mode mem =
    case mem.input of
        [] ->
            Waiting mem

        x :: xs ->
            setValue mode (big x) { mem | input = xs }


doOutput : BigInt -> Memory -> OpResult
doOutput value mem =
    case toInt value of
        Nothing ->
            Fail "Value is too big to output" (Just mem)

        Just v ->
            Next <|
                consumeRegisters
                    { mem
                        | output = mem.output ++ [ v ]
                        , instruction = ReadCode
                    }


jumpIf : (BigInt -> Bool) -> BigInt -> BigInt -> Memory -> OpResult
jumpIf func value jumpPos mem =
    if func value then
        case toInt jumpPos of
            Nothing ->
                Fail "Jump-to position too big" (Just mem)

            Just jumpPosInt ->
                Next <| nextInstruction <| consumeRegisters { mem | pos = jumpPosInt }

    else
        Next <| consumeRegisters { mem | instruction = ReadCode }


changeBase : BigInt -> Memory -> OpResult
changeBase newBase mem =
    case toInt newBase of
        Nothing ->
            Fail "New base too big" (Just mem)

        Just val ->
            Next <| nextInstruction <| consumeRegisters { mem | base = mem.base + val }



-- PROGRAM CONTROL


start : List Int -> List Int -> OpResult
start program inputs =
    Next (initMemory program inputs)


run : List Int -> List Int -> OpResult
run program inputs =
    start program inputs |> runThrough


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
            runThrough <| Next { mem | input = input }

        Next _ ->
            runThrough state

        Done { output } ->
            Fail
                ("Already Done, with output " ++ intsToString output)
                Nothing

        Fail _ _ ->
            state


consumeOutput : OpResult -> ( List Int, OpResult )
consumeOutput state =
    case state of
        Waiting mem ->
            ( mem.output, Waiting { mem | output = [] } )

        Next mem ->
            ( mem.output, Next { mem | output = [] } )

        Done done ->
            ( done.output, state )

        Fail _ _ ->
            ( [], state )
