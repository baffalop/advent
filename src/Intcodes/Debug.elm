module Intcodes.Debug exposing (LooseInstruction(..), continueUntil, stepThrough, viewState)

import Array
import BigInt
import Intcodes.Intcodes exposing (Instruction(..), Memory, OpResult(..), step)


type ViewableOpResult
    = VDone { output : List Int, finalState : List String }
    | VNext VMemory
    | VWaiting VMemory
    | VFail String (Maybe VMemory)


type alias VMemory =
    { ar : List String
    , pos : Int
    , base : Int
    , input : List Int
    , output : List Int
    , instruction : Instruction
    , registers : List String
    , immediateValue : Maybe String
    , positionalValue : Maybe String
    , relativeValue : Maybe String
    }


type LooseInstruction
    = LReadCode
    | LArithmetic
    | LInput
    | LOutput
    | LJumpIf
    | LChangeBase


viewMemory : Memory -> VMemory
viewMemory { ar, pos, base, input, output, instruction, registers, immediateValue, positionalValue, relativeValue } =
    { ar = Array.toList ar |> List.map BigInt.toString
    , pos = pos
    , base = base
    , input = input
    , output = output
    , instruction = instruction
    , registers = List.map BigInt.toString registers
    , immediateValue = Maybe.map BigInt.toString immediateValue
    , positionalValue = Maybe.map BigInt.toString positionalValue
    , relativeValue = Maybe.map BigInt.toString relativeValue
    }


viewState : OpResult -> ViewableOpResult
viewState result =
    case result of
        Done { output, finalState } ->
            VDone
                { output = output
                , finalState = List.map BigInt.toString finalState
                }

        Next mem ->
            VNext (viewMemory mem)

        Waiting mem ->
            VWaiting (viewMemory mem)

        Fail msg mem ->
            VFail msg (Maybe.map viewMemory mem)


matchInstruction : Instruction -> LooseInstruction -> Bool
matchInstruction exactInst looseInst =
    case exactInst of
        ReadCode ->
            looseInst == LReadCode

        Arithmetic _ _ ->
            looseInst == LArithmetic

        JumpIf _ _ ->
            looseInst == LJumpIf

        Input _ ->
            looseInst == LInput

        Output _ ->
            looseInst == LOutput

        ChangeBase _ ->
            looseInst == LChangeBase

        _ ->
            False


continueUntil : LooseInstruction -> OpResult -> OpResult
continueUntil instruction state =
    let
        nextState =
            step state
    in
    case nextState of
        Next mem ->
            if matchInstruction mem.instruction instruction then
                nextState

            else
                continueUntil instruction nextState

        _ ->
            nextState


stepThrough : Int -> OpResult -> ViewableOpResult
stepThrough n state =
    case n of
        0 ->
            state |> viewState

        1 ->
            step state |> viewState

        _ ->
            stepThrough (n - 1) (step state)
