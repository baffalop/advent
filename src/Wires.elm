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


unpackMove : Move -> ( Direction, Int )
unpackMove move =
    case move of
        U count ->
            ( Up, count )

        D count ->
            ( Up, -count )

        R count ->
            ( Right, count )

        L count ->
            ( Right, -count )


addVector : Coord -> Vector -> Coord
addVector origin vector =
    case vector of
        Right count ->
            Tuple.mapFirst ((+) count)

        Up count ->
            Tuple.mapSecond ((+) count)


addMove : Move -> Coord -> Coord
addMove move origin =
    let
        vector =
            case unpackMove move of
                ( Right, x ) ->
                    ( x, 0 )

                ( Up, y ) ->
                    ( 0, y )
    in
    addCoord origin vector


expandMove : Move -> Set Coord
expandMove move =
    let
        unpackedMove =
            unpackMove move

        moveCount =
            Tuple.second unpackedMove

        range =
            if moveCount > 0 then
                List.range 1 moveCount

            else
                List.range moveCount -1
                    |> List.reverse

        toCoord =
            case Tuple.first unpackedMove of
                Up ->
                    Tuple.pair 0

                Right ->
                    \x -> ( x, 0 )
    in
    List.map toCoord range
        |> Set.fromList


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
