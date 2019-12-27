module Asteroids.Asteroids exposing (bestStation, parse, visibleFrom)

import Set exposing (Set)


type alias Coord =
    ( Int, Int )


parse : String -> Set Coord
parse input =
    let
        parseChar char ( ( x, y ), asteroids ) =
            case char of
                '\n' ->
                    ( ( 0, y + 1 ), asteroids )

                '#' ->
                    ( ( x + 1, y ), Set.insert ( x, y ) asteroids )

                '.' ->
                    ( ( x + 1, y ), asteroids )

                _ ->
                    ( ( x, y ), asteroids )
    in
    String.toList input
        |> List.foldl parseChar ( ( 0, 0 ), Set.empty )
        |> Tuple.second


findBounds : Set Coord -> Coord
findBounds asteroids =
    Set.foldl (\( x, y ) ( a, b ) -> ( max x a, max y b )) ( 0, 0 ) asteroids


findOcclusions : Coord -> Coord -> Coord -> Set Coord
findOcclusions ( a, b ) ( boundX, boundY ) ( x, y ) =
    let
        increment =
            reduce ( x - a, y - b )

        addOcclusion point occlusions =
            let
                (( nextX, nextY ) as next) =
                    addCoord increment point
            in
            if nextX < 0 || nextX > boundX || nextY < 0 || nextY > boundY then
                occlusions

            else
                addOcclusion next (Set.insert next occlusions)
    in
    addOcclusion ( x, y ) Set.empty


addCoord : Coord -> Coord -> Coord
addCoord ( a, b ) ( x, y ) =
    ( a + x, b + y )


reduce : Coord -> Coord
reduce ( x, y ) =
    if x == 0 || y == 0 then
        ( x // abs x, y // abs y )

    else
        let
            divisor =
                gcd (abs x) (abs y)
        in
        ( x // divisor, y // divisor )


gcd : Int -> Int -> Int
gcd x y =
    let
        greater =
            max x y

        lesser =
            min x y
    in
    case lesser of
        0 ->
            greater

        _ ->
            gcd lesser (modBy lesser greater)


visibleFrom : Set Coord -> Coord -> Set Coord
visibleFrom asteroids station =
    let
        bounds =
            findBounds asteroids

        excludingStation =
            Set.remove station asteroids

        addOcclusions point occlusions =
            findOcclusions station bounds point |> Set.union occlusions
    in
    Set.foldl addOcclusions Set.empty excludingStation
        |> Set.diff excludingStation


bestStation : Set Coord -> Maybe ( Coord, Int )
bestStation asteroids =
    let
        stationCandidates =
            Set.toList asteroids
    in
    List.map (visibleFrom asteroids >> Set.size) stationCandidates
        |> List.map2 Tuple.pair stationCandidates
        |> List.sortBy (Tuple.second >> negate)
        |> List.head
