module Printer exposing (print)

import Array exposing (Array)
import Dict exposing (Dict)
import Dict.Extra


type alias Vector =
    ( Int, Int )


findPaintedBounds : Dict Vector a -> ( Vector, Vector )
findPaintedBounds painted =
    let
        listVectors =
            Dict.keys painted

        minCoord =
            List.foldl (\( x, y ) ( a, b ) -> ( min x a, min y b )) ( 0, 0 ) listVectors

        maxCoord =
            List.foldl (\( x, y ) ( a, b ) -> ( max x a, max y b )) ( 0, 0 ) listVectors
    in
    ( minCoord, maxCoord )


prepairCanvas : ( Vector, Vector ) -> Array (Array Char)
prepairCanvas ( ( x1, y1 ), ( x2, y2 ) ) =
    let
        xRange =
            x2 - x1 + 1

        yRange =
            y2 - y1 + 1
    in
    Array.repeat yRange (Array.repeat xRange '.')


print : (a -> Char) -> Dict Vector a -> String
print brush input =
    let
        drawables =
            Dict.filter (\_ v -> brush v /= '.') input

        (( ( lowerX, lowerY ), _ ) as bounds) =
            findPaintedBounds drawables

        normalisedInput =
            Dict.Extra.mapKeys (\( x, y ) -> ( x - lowerX, y - lowerY )) drawables

        daub ( x, y ) val canvas =
            let
                row =
                    Array.get y canvas |> Maybe.withDefault Array.empty
            in
            Array.set y (Array.set x (brush val) row) canvas
    in
    Dict.foldl daub (prepairCanvas bounds) normalisedInput
        |> Array.map Array.toList
        |> Array.toList
        |> List.intersperse [ '\n' ]
        |> List.concat
        |> String.fromList
