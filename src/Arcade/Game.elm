module Arcade.Game exposing (..)

import Intcodes.Intcodes as IC exposing (OpResult(..))


type alias Tile =
    { loc : ( Int, Int )
    , tileType : TileType
    }


type TileType
    = Unrecognised
    | Empty
    | Wall
    | Block
    | Paddle
    | Ball


parseOutput : List Int -> ( List Tile, List Int )
parseOutput output =
    case output of
        x :: y :: tile :: rest ->
            let
                ( tiles, leftovers ) =
                    parseOutput rest
            in
            ( { loc = ( x, y ), tileType = parseTile tile } :: tiles, leftovers )

        leftovers ->
            ( [], leftovers )


parseTile : Int -> TileType
parseTile input =
    case input of
        0 ->
            Empty

        1 ->
            Wall

        2 ->
            Block

        3 ->
            Paddle

        4 ->
            Ball

        _ ->
            Unrecognised
