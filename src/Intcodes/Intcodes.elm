module Intcodes.Intcodes exposing (Instruction(..), Memory, OpResult(..), continue, run, start, step)

import Array exposing (Array)
import BigInt exposing (BigInt, add, gt, lt, mul)
import Utils exposing (flip, intsToString)


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
    , registers : List BigInt
    , immediateValue : Maybe BigInt
    , positionalValue : Maybe BigInt
    , relativeValue : Maybe BigInt
    }


type OpResult
    = Done { output : List Int, finalState : List BigInt }
    | Next Memory
    | Waiting Memory
    | Fail String (Maybe Memory)


initMemory : List Int -> List Int -> Memory
initMemory program inputs =
    { ar = Array.fromList <| List.map BigInt.fromInt program
    , pos = 0
    , base = 0
    , input = inputs
    , output = []
    , instruction = ReadCode
    , registers = []
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
    { mem | registers = [] }


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
                    case List.length registers of
                        0 ->
                            getValue mode1 pos mem

                        1 ->
                            getValue mode2 pos mem

                        _ ->
                            doArithmetic mode3 f mem

                JumpIf f ( mode1, mode2 ) ->
                    case List.length registers of
                        0 ->
                            getValue mode1 pos mem

                        1 ->
                            getValue mode2 pos mem

                        _ ->
                            jumpIf f mem

                Input mode ->
                    doInput mode mem

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


big : Int -> BigInt
big =
    BigInt.fromInt


toInt : BigInt -> Maybe Int
toInt n =
    if gt n (big 9007199254740991) then
        Nothing

    else
        n |> BigInt.toString |> String.toInt


eq : BigInt -> BigInt -> Bool
eq x y =
    not (lt x y) && not (gt x y)


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


doArithmetic : Mode -> (BigInt -> BigInt -> BigInt) -> Memory -> OpResult
doArithmetic mode op mem =
    case mem.registers of
        [] ->
            Fail "Registers are not populated" (Just mem)

        _ :: [] ->
            Fail "Registers are not populated" (Just mem)

        x :: (y :: _) ->
            setValue mode (op x y) (consumeRegisters mem)


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
                    next { newMem | registers = mem.registers ++ [ value ] }

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


doOutput : Memory -> OpResult
doOutput mem =
    case List.head mem.registers of
        Nothing ->
            Fail "Registers not populated" (Just mem)

        Just value ->
            case toInt value of
                Nothing ->
                    Fail "Value is too big to output" (Just mem)

                Just v ->
                    Next
                        (consumeRegisters
                            { mem
                                | output = mem.output ++ [ v ]
                                , instruction = ReadCode
                            }
                        )


jumpIf : (BigInt -> Bool) -> Memory -> OpResult
jumpIf func mem =
    case mem.registers of
        [] ->
            Fail "Registers not populated" (Just mem)

        _ :: [] ->
            Fail "Registers not populated" (Just mem)

        value :: (jumpPos :: _) ->
            if func value then
                case toInt jumpPos of
                    Nothing ->
                        Fail "Jump-to position too big" (Just mem)

                    Just jumpPosInt ->
                        Next (nextInstruction <| consumeRegisters { mem | pos = jumpPosInt })

            else
                Next (consumeRegisters { mem | instruction = ReadCode })


changeBase : Memory -> OpResult
changeBase mem =
    case List.head mem.registers of
        Nothing ->
            Fail "Registers not populated" (Just mem)

        Just value ->
            case toInt value of
                Nothing ->
                    Fail "New base too big" (Just mem)

                Just baseChange ->
                    Next (nextInstruction <| consumeRegisters { mem | base = mem.base + baseChange })


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
            runThrough <| Next { mem | input = input }

        Next _ ->
            runThrough state

        Done { output } ->
            Fail
                ("Already Done, with output " ++ intsToString output)
                Nothing

        Fail _ _ ->
            state
