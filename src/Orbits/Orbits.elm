module Orbits.Orbits exposing (Map, countOrbits, findPath, orbitalTransfersPath, parseMap)

import Dict exposing (Dict)
import Orbits.Parser as Parser
import Utils exposing (flip)


type alias Mapping =
    ( String, String )


type alias Map =
    Dict String String


mapFromList : List Mapping -> Map
mapFromList =
    Dict.fromList << List.map (\( x, y ) -> ( y, x ))


countOrbits : Map -> Int
countOrbits map =
    Dict.toList map
        |> List.map (Tuple.first >> countToCom map)
        |> List.sum


countToCom : Map -> String -> Int
countToCom map name =
    case Dict.get name map of
        Nothing ->
            1

        Just root ->
            1 + countToCom map root


parseMap : String -> Map
parseMap input =
    Parser.parse input
        |> Result.withDefault []
        |> mapFromList


type alias Jump =
    ( Direction, String )


type Direction
    = Up
    | Down


searchDown : Mapping -> Map -> List String
searchDown ( from, to ) map =
    case Dict.get from map of
        Nothing ->
            []

        Just next ->
            if next == to then
                [ to ]

            else
                next :: searchDown ( next, to ) map


findPath : Mapping -> Map -> Maybe (List Jump)
findPath ( from, to ) map =
    let
        down =
            searchDown ( from, to ) map

        up =
            (to :: searchDown ( to, from ) map)
                |> List.reverse
                |> List.tail
                |> Maybe.withDefault []

        toJumps =
            \dir -> List.map (Tuple.pair dir)
    in
    case Utils.littleTail down of
        Just bottom ->
            if bottom == to then
                Just (toJumps Down down)

            else
                let
                    intersect =
                        List.filter (flip List.member up) down
                in
                Maybe.map
                    (\junction ->
                        (Utils.takeTo junction down |> toJumps Down)
                            ++ (Utils.dropTo junction up |> toJumps Up)
                    )
                    (List.head intersect)

        Nothing ->
            Maybe.andThen
                (\bottom ->
                    if bottom == from then
                        Just (toJumps Up up)

                    else
                        Nothing
                )
                (List.head up)


orbitalTransfersPath : Mapping -> Map -> Maybe (List Jump)
orbitalTransfersPath ( from, to ) map =
    Maybe.map2 Tuple.pair (Dict.get from map) (Dict.get to map)
        |> Maybe.andThen (flip findPath map)
