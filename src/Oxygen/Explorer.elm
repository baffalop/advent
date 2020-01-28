module Oxygen.Explorer exposing (countStepsToOxygen, explore)

import Dict exposing (Dict)
import Oxygen.Robot as Robot exposing (Direction(..), Robot)
import Set exposing (Set)
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


exploreStep : Explorer Robot -> Result String (Explorer Robot)
exploreStep explorer =
    case explorer.step of
        ProbeLeft ->
            probeLeft explorer

        ProbeForward ->
            probeForward explorer


exploreAll : Result String (Explorer Robot) -> Result String Robot.Map
exploreAll =
    Result.andThen
        (\explorer ->
            if explorer.map.droidLocation == ( 0, 0 ) && adjacentsAreKnown explorer.map then
                Ok explorer.map

            else
                exploreAll (exploreStep explorer)
        )


explore : Result String Robot.Map
explore =
    exploreAll (Ok init)



-- FIND OXYGEN


hasFoundOxygen : Robot.Map -> Bool
hasFoundOxygen { droidLocation, surroundings } =
    Dict.get droidLocation surroundings == Just Robot.Oxygen


trackStepsToOxygen : Set ( Int, Int ) -> Result String (Explorer Robot) -> Result String Int
trackStepsToOxygen steps explorerResult =
    case explorerResult of
        Err msg ->
            Err msg

        Ok explorer ->
            if hasFoundOxygen explorer.map then
                Ok (Set.size steps)

            else
                let
                    nextStep =
                        exploreStep explorer

                    currentLocation =
                        explorer.map.droidLocation

                    nextLocation =
                        Result.withDefault init nextStep
                            |> .map
                            |> .droidLocation

                    nextSteps =
                        if nextLocation /= currentLocation && Set.member nextLocation steps then
                            Set.remove currentLocation steps

                        else
                            Set.insert nextLocation steps
                in
                trackStepsToOxygen nextSteps nextStep


countStepsToOxygen : Result String Int
countStepsToOxygen =
    trackStepsToOxygen Set.empty (Ok init)
