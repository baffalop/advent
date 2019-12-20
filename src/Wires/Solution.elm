module Wires.Solution exposing (Move(..), closestManhattanCrossing, closestWireCrossing, composeMoves, findCrossings)

import Utils exposing (filterMaybes)


type Move
    = U Int
    | D Int
    | L Int
    | R Int


type Direction
    = Up
    | Right


type alias Vector =
    ( Direction, Int )


type alias Coord =
    ( Int, Int )


type alias Edge =
    { origin : Coord
    , vector : Vector
    , distance : Int
    }


toVector : Move -> Vector
toVector move =
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
addVector origin ( dir, count ) =
    case dir of
        Right ->
            Tuple.mapFirst ((+) count) origin

        Up ->
            Tuple.mapSecond ((+) count) origin


vectorsToEdges : Coord -> Int -> List Vector -> List Edge
vectorsToEdges origin distance input =
    case input of
        [] ->
            []

        v :: vs ->
            let
                edge =
                    { origin = origin
                    , vector = v
                    , distance = distance
                    }

                endpoint =
                    addVector origin v

                nextDistance =
                    distance + abs (Tuple.second v)
            in
            edge :: vectorsToEdges endpoint nextDistance vs


composeMoves : List Move -> List Edge
composeMoves moves =
    List.map toVector moves
        |> vectorsToEdges ( 0, 0 ) 0


sortPerpendicular : Edge -> Edge -> Maybe ( Edge, Edge )
sortPerpendicular e1 e2 =
    let
        dir1 =
            Tuple.first e1.vector

        dir2 =
            Tuple.first e2.vector
    in
    case ( dir1, dir2 ) of
        ( Up, Up ) ->
            Nothing

        ( Right, Right ) ->
            Nothing

        ( Right, Up ) ->
            Just ( e1, e2 )

        ( Up, Right ) ->
            Just ( e2, e1 )


findCrossing : Edge -> Edge -> Maybe ( Coord, Int )
findCrossing e1 e2 =
    case sortPerpendicular e1 e2 of
        Nothing ->
            Nothing

        Just ( horiz, vert ) ->
            let
                ( xh, yh ) =
                    horiz.origin

                ( _, right ) =
                    horiz.vector

                ( xv, yv ) =
                    vert.origin

                ( _, up ) =
                    vert.vector

                distance =
                    abs (yh - yv)
                        + vert.distance
                        + abs (xv - xh)
                        + horiz.distance

                between =
                    \a ( x, y ) -> a > min x y && a < max x y
            in
            if
                between yh ( yv, yv + up )
                    && between xv ( xh, xh + right )
            then
                Just ( ( xv, yh ), distance )

            else
                Nothing


findCrossings : List Move -> List Move -> List ( Coord, Int )
findCrossings moves1 moves2 =
    let
        edges1 =
            composeMoves moves1

        edges2 =
            composeMoves moves2
    in
    List.concatMap (\e -> List.map (findCrossing e) edges2) edges1
        |> filterMaybes


closestCrossing : (( Coord, Int ) -> Int) -> List Move -> List Move -> Maybe { coord : Coord, distance : Int }
closestCrossing measure moves1 moves2 =
    let
        crossings =
            findCrossings moves1 moves2
                |> List.sortBy measure

        closest =
            List.head crossings
    in
    Maybe.map
        (\crossing ->
            { coord = Tuple.first crossing
            , distance = measure crossing
            }
        )
        closest


closestManhattanCrossing : List Move -> List Move -> Maybe { coord : Coord, distance : Int }
closestManhattanCrossing =
    let
        manhattanDistance =
            \( x, y ) -> abs x + abs y
    in
    closestCrossing (Tuple.first >> manhattanDistance)


closestWireCrossing : List Move -> List Move -> Maybe { coord : Coord, distance : Int }
closestWireCrossing =
    closestCrossing Tuple.second
