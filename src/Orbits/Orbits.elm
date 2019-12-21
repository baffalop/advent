module Orbits.Orbits exposing (countOrbits, parseMap, sortMappings)

import Dict exposing (Dict)
import Orbits.Parser as Parser exposing (Mapping)


type alias Map =
    { moons : Dict String (List String)
    , orbits : Dict String String
    }


mapFromList : List Mapping -> Map
mapFromList mappings =
    let
        addMoon =
            \( com, moon ) map ->
                if Dict.member com map then
                    Dict.update com (Maybe.map ((::) moon)) map

                else
                    Dict.insert com [ moon ] map
    in
    { orbits = Dict.fromList <| List.map (\( x, y ) -> ( y, x )) mappings
    , moons = List.foldl addMoon Dict.empty mappings
    }


sortMappings : List Mapping -> List Mapping
sortMappings mappings =
    let
        map =
            mapFromList mappings
    in
    List.sortBy (Tuple.first >> countNodesFromCom map) mappings


countOrbits : Map -> Int
countOrbits map =
    Dict.toList map.orbits
        |> List.map (Tuple.first >> countNodesFromCom map)
        |> List.sum


countNodesFromCom : Map -> String -> Int
countNodesFromCom map name =
    case Dict.get name map.orbits of
        Nothing ->
            1

        Just root ->
            1 + countNodesFromCom map root


parseMap : String -> Map
parseMap input =
    Parser.parseMap input
        |> Result.withDefault []
        |> mapFromList
