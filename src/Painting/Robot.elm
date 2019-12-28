module Painting.Robot exposing (run)

import Dict exposing (Dict)
import Intcodes.Intcodes as Intcodes exposing (OpResult(..), continue)
import Utils exposing (intsToString)


type alias Vector =
    ( Int, Int )


type Colour
    = Black
    | White


type Direction
    = Up
    | Right
    | Down
    | Left


type alias RobotState =
    { program : OpResult
    , heading : Direction
    , location : Vector
    , painted : Dict Vector Colour
    }


type Robot
    = Running RobotState
    | Finished { painted : Dict Vector Colour }
    | Stalled String RobotState


initRobot : List Int -> Robot
initRobot program =
    Running
        { program = Intcodes.run program []
        , heading = Up
        , location = ( 0, 0 )
        , painted = Dict.empty
        }


andThen : (RobotState -> Robot) -> Robot -> Robot
andThen f robot =
    case robot of
        Running state ->
            f state

        _ ->
            robot


turn : Int -> RobotState -> Robot
turn instruction ({ heading } as state) =
    case ( heading, instruction ) of
        ( Up, 0 ) ->
            Running { state | heading = Left }

        ( Up, 1 ) ->
            Running { state | heading = Right }

        ( Right, 0 ) ->
            Running { state | heading = Up }

        ( Right, 1 ) ->
            Running { state | heading = Down }

        ( Down, 0 ) ->
            Running { state | heading = Right }

        ( Down, 1 ) ->
            Running { state | heading = Left }

        ( Left, 0 ) ->
            Running { state | heading = Down }

        ( Left, 1 ) ->
            Running { state | heading = Up }

        ( _, _ ) ->
            Stalled ("Invalid turn instruction " ++ String.fromInt instruction) state


forward : RobotState -> Robot
forward ({ heading, location } as state) =
    let
        ( x, y ) =
            location

        newLocation =
            case heading of
                Up ->
                    ( x, y - 1 )

                Right ->
                    ( x + 1, y )

                Down ->
                    ( x, y + 1 )

                Left ->
                    ( x - 1, y )
    in
    Running { state | location = newLocation }


paint : Int -> RobotState -> Robot
paint instruction ({ painted, location } as state) =
    let
        paintColour =
            case instruction of
                0 ->
                    Just Black

                1 ->
                    Just White

                _ ->
                    Nothing
    in
    case paintColour of
        Nothing ->
            Stalled ("Invalid colour instruction " ++ String.fromInt instruction) state

        Just colour ->
            if Dict.member location painted then
                Running
                    { state
                        | painted = Dict.update location (Maybe.map <| always colour) painted
                    }

            else
                Running
                    { state
                        | painted = Dict.insert location colour painted
                    }


inputColour : RobotState -> Robot
inputColour ({ painted, location, program } as state) =
    let
        colour =
            Dict.get location painted
                |> Maybe.withDefault Black

        colourInput =
            case colour of
                Black ->
                    0

                White ->
                    1
    in
    Running { state | program = continue program [ colourInput ] }


handleOutput : RobotState -> Robot
handleOutput ({ program } as state) =
    case program of
        Done _ ->
            Finished { painted = state.painted }

        Fail _ _ ->
            Stalled "Intcodes failed" state

        Next _ ->
            Running { state | program = continue program [] }
                |> andThen handleOutput

        Waiting ({ output } as programState) ->
            case output of
                paintInstruction :: turnInstruction :: [] ->
                    Running { state | program = Waiting { programState | output = [] } }
                        |> andThen (paint paintInstruction)
                        |> andThen (turn turnInstruction)

                _ :: _ :: _ ->
                    Stalled ("Received too many outputs " ++ intsToString output) state

                x :: [] ->
                    Stalled ("Only received one output " ++ String.fromInt x) state

                [] ->
                    Stalled "No output" state


step : Robot -> Robot
step =
    andThen inputColour
        >> andThen handleOutput
        >> andThen forward


runThrough : Robot -> Robot
runThrough robot =
    case robot of
        Running _ ->
            runThrough (step robot)

        _ ->
            robot


run : List Int -> Robot
run program =
    initRobot program |> runThrough
