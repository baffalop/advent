module Wires exposing (Move(..), composeMoves, crossings)

import Set exposing (Set)


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


addCoord : Coord -> Coord -> Coord
addCoord ( x, y ) =
    Tuple.mapBoth ((+) x) ((+) y)


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


filterMaybes : List (Maybe a) -> List a
filterMaybes list =
    case list of
        [] ->
            []

        x :: xs ->
            case x of
                Nothing ->
                    filterMaybes xs

                Just a ->
                    a :: filterMaybes xs


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
                Just ( ( xh, yv ), distance )

            else
                Nothing


crossings : List Move -> List Move -> List ( Coord, Int )
crossings moves1 moves2 =
    let
        edges1 =
            composeMoves moves1

        edges2 =
            composeMoves moves2
    in
    List.concatMap (\e -> List.map (findCrossing e) edges2) edges1
        |> filterMaybes
        |> List.sortBy Tuple.second
