module Oxygen.Expander exposing (expand)

import Dict exposing (Dict)
import Oxygen.Robot as Robot exposing (Direction(..), Element(..))


type alias Map =
    Dict ( Int, Int ) Element


adjacents : Map -> ( Int, Int ) -> List ( ( Int, Int ), Maybe Element )
adjacents map coord =
    List.map
        (Robot.applyDirection coord
            >> (\coord_ -> ( coord_, Dict.get coord_ map ))
        )
        [ North, East, South, West ]


adjacentsWithSpace : Map -> ( Int, Int ) -> List ( Int, Int )
adjacentsWithSpace map coord =
    List.filter
        (Tuple.second >> (==) (Just Space))
        (adjacents map coord)
        |> List.map Tuple.first


initOxygen : Map -> List ( Int, Int )
initOxygen map =
    Dict.filter (\_ elem -> elem == Oxygen) map
        |> Dict.toList
        |> List.map Tuple.first


addOxygen : Map -> List ( Int, Int ) -> Map
addOxygen =
    List.foldl (\coord newMap -> Dict.insert coord Oxygen newMap)


expandOxygen : Map -> List ( Int, Int ) -> List ( Int, Int )
expandOxygen map =
    List.concatMap (adjacentsWithSpace map)


expand : Map -> Int
expand initialMap =
    let
        countExpansions : Map -> List ( Int, Int ) -> Int
        countExpansions map oxygen =
            let
                newOxygen =
                    expandOxygen map oxygen
            in
            if newOxygen == [] then
                0

            else
                1 + countExpansions (addOxygen map oxygen) newOxygen
    in
    initOxygen initialMap
        |> countExpansions initialMap
