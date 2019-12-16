module Wires exposing (Move(..), expandMoves, input1, input2, minManhattan)

import Set exposing (Set)


type Move
    = U Int
    | D Int
    | L Int
    | R Int


type Vector
    = Up Int
    | Right Int


type alias Coord =
    ( Int, Int )


type alias Edge =
    { origin : Coord
    , vector : Vector
    , distance : Int
    }


addCoord : Coord -> Coord -> Coord
addCoord ( x, y ) =
    Tuple.mapBoth ((+) x) ((+) y)


toVector : Move -> Vector
toVector move =
    case move of
        U count ->
            Up count

        D count ->
            Up -count

        R count ->
            Right count

        L count ->
            Right -count


addVector : Coord -> Vector -> Coord
addVector origin vector =
    case vector of
        Right count ->
            Tuple.mapFirst ((+) count)

        Up count ->
            Tuple.mapSecond ((+) count)


expandMoves : List Move -> Set Coord
expandMoves input =
    case input of
        [] ->
            Set.empty

        move :: moves ->
            expandMoves moves
                |> Set.map (addMove move)
                |> Set.union (expandMove move)


minManhattan : Set Coord -> List Coord
minManhattan coords =
    Set.toList coords
        |> List.sortBy (\( x, y ) -> abs x + abs y)
