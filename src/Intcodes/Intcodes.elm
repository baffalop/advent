module Intcodes.Intcodes exposing (OpResult(..), continue, run)

import Array exposing (Array)
import Utils exposing (flip, intsToString)


type Opcode
    = Add ( Mode, Mode )
    | Mult ( Mode, Mode )
    | Input
    | Output Mode
    | JumpIfTrue ( Mode, Mode )
    | JumpIfFalse ( Mode, Mode )
    | LessThan ( Mode, Mode )
    | Equals ( Mode, Mode )
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
    }


initMemory : List Int -> List Int -> Memory
initMemory program inputs =
    { ar = Array.fromList program
    , pos = 0
    , base = 0
    , input = inputs
    , output = []
    }


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

        2 ->
            Just ( Relative, next )

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

        9 ->
            with1Mode (\m -> ChangeBase m)

        99 ->
            Halt

        _ ->
            Unrecognised


type OpResult
    = Done { output : List Int, finalState : List Int }
    | Waiting Memory
    | Fail String (Maybe Memory)


doNextOpcode : Memory -> OpResult
doNextOpcode mem =
    case Array.get mem.pos mem.ar of
        Nothing ->
            Fail "Opcode out of bounds" (Just mem)

        Just code ->
            case readOpcode code of
                Unrecognised ->
                    Fail ("Unrecognised code " ++ String.fromInt code) (Just mem)

                Halt ->
                    Done
                        { output = mem.output
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

                ChangeBase mode ->
                    changeBase mode mem


doArithmetic : (Int -> Int -> Int) -> ( Mode, Mode ) -> Memory -> OpResult
doArithmetic op ( mode1, mode2 ) mem =
    let
        arg1 =
            getValue mode1 (mem.pos + 1) mem

        arg2 =
            getValue mode2 (mem.pos + 2) mem

        applyOp ( x, _ ) ( y, newMem ) =
            ( op x y, newMem )
    in
    case Maybe.map2 applyOp arg1 arg2 of
        Nothing ->
            Fail "Argument out of bounds" (Just mem)

        Just ( result, newMem ) ->
            setValue 3 result newMem


getValue : Mode -> Int -> Memory -> Maybe ( Int, Memory )
getValue mode pos mem =
    let
        expandedAr =
            expandArray mem.ar pos

        immediateValue =
            Array.get pos expandedAr

        nextMem =
            { mem | ar = expandedAr }
    in
    case mode of
        Immediate ->
            Maybe.map (flip Tuple.pair nextMem) immediateValue

        Position ->
            Maybe.andThen (\i -> getValue Immediate i nextMem) immediateValue

        Relative ->
            Maybe.andThen (\i -> getValue Immediate (mem.base + i) nextMem) immediateValue


expandArray : Array Int -> Int -> Array Int
expandArray ar toPos =
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


setValue : Int -> Int -> Memory -> OpResult
setValue offset val mem =
    let
        pos =
            mem.pos + offset

        writePosition =
            Array.get pos mem.ar
    in
    case writePosition of
        Nothing ->
            Fail "Param out of bounds" (Just mem)

        Just i ->
            if i < 0 then
                Fail "Write position is negative" (Just mem)

            else
                doNextOpcode
                    { mem
                        | ar =
                            Array.set i val <|
                                expandArray mem.ar i
                        , pos = pos + 1
                    }


doInput : Memory -> OpResult
doInput mem =
    case mem.input of
        [] ->
            Waiting mem

        x :: xs ->
            setValue 1 x { mem | input = xs }


doOutput : Mode -> Memory -> OpResult
doOutput mode mem =
    case getValue mode (mem.pos + 1) mem of
        Nothing ->
            Fail "Argument out of bounds" (Just mem)

        Just ( value, nextMem ) ->
            doNextOpcode
                { nextMem
                    | output = mem.output ++ [ value ]
                    , pos = mem.pos + 2
                }


jumpIf : ( Mode, Mode ) -> (Bool -> Bool) -> Memory -> OpResult
jumpIf ( mode1, mode2 ) func mem =
    case getValue mode1 (mem.pos + 1) mem of
        Nothing ->
            Fail "Argument out of bounds" (Just mem)

        Just ( value, newMem ) ->
            if func (value /= 0) then
                case getValue mode2 (mem.pos + 2) newMem of
                    Nothing ->
                        Fail "Argument out of bounds" (Just mem)

                    Just ( v, nextMem ) ->
                        doNextOpcode { nextMem | pos = v }

            else
                doNextOpcode { newMem | pos = newMem.pos + 3 }


changeBase : Mode -> Memory -> OpResult
changeBase mode mem =
    case getValue mode (mem.pos + 1) mem of
        Nothing ->
            Fail "Argument out of bounds" (Just mem)

        Just ( newBase, nextMem ) ->
            doNextOpcode
                { nextMem
                    | base = newBase
                    , pos = mem.pos + 2
                }


run : List Int -> List Int -> OpResult
run program inputs =
    initMemory program inputs |> doNextOpcode


continue : OpResult -> List Int -> OpResult
continue state input =
    case state of
        Waiting mem ->
            doInput { mem | input = input }

        Done { output } ->
            Fail
                ("Already Done, with output " ++ intsToString output)
                Nothing

        _ ->
            state
