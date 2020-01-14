module Factory.Factory exposing (..)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser)
import Set


type alias Price =
    ( Int, String )


type alias Reactions =
    Dict String ( Int, List Price )



-- Parser


priceParser : Parser Price
priceParser =
    Parser.succeed Tuple.pair
        |= Parser.int
        |. Parser.spaces
        |= Parser.variable
            { start = Char.isAlpha
            , inner = Char.isAlpha
            , reserved = Set.empty
            }


pricesParser : Parser (List Price)
pricesParser =
    Parser.sequence
        { start = ""
        , end = ""
        , separator = ","
        , spaces = Parser.spaces
        , item = priceParser
        , trailing = Parser.Forbidden
        }


reactionParser : Parser { name : String, qty : Int, prices : List Price }
reactionParser =
    Parser.succeed (\prices qty name -> { name = name, qty = qty, prices = prices })
        |= pricesParser
        |. Parser.spaces
        |. Parser.symbol "=>"
        |. Parser.spaces
        |= Parser.int
        |. Parser.spaces
        |= Parser.variable
            { start = Char.isAlpha
            , inner = Char.isAlpha
            , reserved = Set.empty
            }


reactionsParser : Parser Reactions
reactionsParser =
    Parser.loop Dict.empty <|
        \reactions ->
            Parser.oneOf
                [ Parser.succeed
                    (\{ name, qty, prices } ->
                        Parser.Loop <| Dict.insert name ( qty, prices ) reactions
                    )
                    |= reactionParser
                    |. Parser.chompWhile ((==) '\n')
                , Parser.succeed ()
                    |> Parser.map (\_ -> Parser.Done reactions)
                ]


parse : String -> Reactions
parse =
    Parser.run reactionsParser
