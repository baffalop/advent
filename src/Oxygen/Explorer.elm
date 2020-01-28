module Oxygen.Explorer exposing (explore)

import Dict exposing (Dict)
import Oxygen.Robot as Robot exposing (Direction(..), Robot)
import Utils exposing (flip)


type alias Explorer robot =
    { robot
        | orientation : Robot.Direction
        , step : RobotStep
    }


type RobotStep
    = ProbeLeft
    | ProbeForward


init : Explorer Robot
init =
    let
        { state, map } =
            Robot.init
    in
    { state = state
    , map = map
    , orientation = North
    , step = ProbeLeft
    }


getRobot : Explorer Robot -> Robot
getRobot explorer =
    { state = explorer.state
    , map = explorer.map
    }


mapRobot : Result String Robot -> Explorer Robot -> Result String (Explorer Robot)
mapRobot robotResult { orientation, step } =
    Result.map
        (\{ state, map } ->
            { state = state
            , map = map
            , orientation = orientation
            , step = step
            }
        )
        robotResult


clockwise : Direction -> Direction
clockwise direction =
    case direction of
        North ->
            East

        East ->
            South

        South ->
            West

        West ->
            North


counterClockwise : Direction -> Direction
counterClockwise direction =
    case direction of
        North ->
            West

        West ->
            South

        South ->
            East

        East ->
            North


probeLeft : Explorer Robot -> Result String (Explorer Robot)
probeLeft explorer =
    let
        leftwards =
            counterClockwise explorer.orientation

        robot =
            getRobot explorer

        moveResults =
            Robot.makeMove leftwards robot

        ( _, feedbackResult ) =
            moveResults

        robotResult =
            Robot.applyMove leftwards robot moveResults
    in
    Result.andThen
        (\feedback ->
            case feedback of
                Robot.Blocked ->
                    mapRobot robotResult { explorer | step = ProbeForward }

                _ ->
                    mapRobot robotResult { explorer | orientation = leftwards }
        )
        feedbackResult


probeForward : Explorer Robot -> Result String (Explorer Robot)
probeForward explorer =
    let
        robot =
            getRobot explorer

        moveResults =
            Robot.makeMove explorer.orientation robot

        ( _, feedbackResult ) =
            moveResults

        robotResult =
            Robot.applyMove explorer.orientation robot moveResults
    in
    Result.andThen
        (\feedback ->
            case feedback of
                Robot.Blocked ->
                    mapRobot robotResult { explorer | orientation = clockwise explorer.orientation }

                _ ->
                    mapRobot robotResult { explorer | step = ProbeLeft }
        )
        feedbackResult


adjacentsAreKnown : Robot.Map -> Bool
adjacentsAreKnown { droidLocation, surroundings } =
    let
        adjacents =
            List.map
                (Robot.applyDirection droidLocation >> flip Dict.get surroundings)
                [ North, East, South, West ]
    in
    List.length adjacents == List.length (Utils.filterMaybes adjacents)


exploreStep : Result String (Explorer Robot) -> Result String Robot.Map
exploreStep =
    Result.andThen
        (\explorer ->
            if explorer.map.droidLocation == ( 0, 0 ) && adjacentsAreKnown explorer.map then
                Ok explorer.map

            else
                case explorer.step of
                    ProbeLeft ->
                        exploreStep (probeLeft explorer)

                    ProbeForward ->
                        exploreStep (probeForward explorer)
        )


explore : Result String Robot.Map
explore =
    exploreStep (Ok init)
