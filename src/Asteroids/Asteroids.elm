module Asteroids.Asteroids exposing (bestStation, nthVaporised, parse)

import Set exposing (Set)
import Utils exposing (filterMaybes)


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
findOcclusions ( a, b ) bounds ( x, y ) =
    let
        increment =
            reduce ( x - a, y - b )

        addOcclusion point occlusions =
            let
                next =
                    addCoord increment point
            in
            if outOfBounds bounds next then
                occlusions

            else
                addOcclusion next (Set.insert next occlusions)
    in
    addOcclusion ( x, y ) Set.empty


outOfBounds : Coord -> Coord -> Bool
outOfBounds ( boundX, boundY ) ( x, y ) =
    x < 0 || x > boundX || y < 0 || y > boundY


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


sortTargetsClockwise : Coord -> Set Coord -> List Coord
sortTargetsClockwise (( a, b ) as centre) asteroids =
    let
        possibleDirections =
            Set.remove centre asteroids |> Set.map ((\( x, y ) -> ( x - a, y - b )) >> reduce)

        sort =
            Set.toList >> List.sortBy (\( x, y ) -> toFloat y / toFloat x)

        quadrant1 =
            Set.filter (\( x, y ) -> x >= 0 && y < 0) possibleDirections
                |> sort

        quadrant2 =
            Set.filter (\( x, y ) -> x >= 0 && y >= 0) possibleDirections
                |> sort

        quadrant3 =
            Set.filter (\( x, y ) -> x < 0 && y > 0) possibleDirections
                |> sort

        quadrant4 =
            Set.filter (\( x, y ) -> x < 0 && y < 0) possibleDirections
                |> sort
    in
    quadrant1 ++ quadrant2 ++ quadrant3 ++ quadrant4


shoot : Coord -> Coord -> Set Coord -> Coord -> Maybe Coord
shoot centre bounds asteroids direction =
    let
        target =
            addCoord centre direction
    in
    if outOfBounds bounds target then
        Nothing

    else if Set.member target asteroids then
        Just target

    else
        shoot target bounds asteroids direction


nthVaporised : Coord -> Set Coord -> Int -> Maybe Coord
nthVaporised centre asteroids n =
    let
        bounds =
            findBounds asteroids

        round =
            sortTargetsClockwise centre asteroids

        fireNextRound asts count =
            let
                astsShot =
                    round
                        |> List.map (shoot centre bounds asts)
                        |> filterMaybes

                countAstsShot =
                    List.length astsShot
            in
            if countAstsShot < 0 then
                Nothing

            else if countAstsShot > count then
                List.drop (count - 1) astsShot |> List.head

            else
                fireNextRound
                    (Set.diff asts (Set.fromList astsShot))
                    (count - countAstsShot + 1)
    in
    fireNextRound asteroids (n - 1)
