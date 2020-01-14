module Factory.Factory exposing (..)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser)
import Set
import Utils exposing (flip)


type alias Price =
    ( String, Int )


type alias Reactions =
    Dict String ( Int, List Price )


expandPrice : Reactions -> Price -> Maybe (List Price)
expandPrice reactions ( name, requiredQty ) =
    Dict.get name reactions
        |> Maybe.map
            (\( acquiredQty, priceList ) ->
                let
                    multiple =
                        ceiling (toFloat requiredQty / toFloat acquiredQty)
                in
                if multiple == 1 then
                    priceList

                else
                    List.map (Tuple.mapSecond ((*) multiple)) priceList
            )


expandPrices : Reactions -> List Price -> Maybe (List Price)
expandPrices reactions prices =
    case prices of
        [] ->
            Just []

        price :: rest ->
            if Tuple.first price == "ORE" then
                Maybe.map ((::) price) (expandPrices reactions rest)

            else
                Maybe.map (flip (++) rest >> consolidatePrices) (expandPrice reactions price)
                    |> Maybe.andThen (expandPrices reactions)


consolidatePrices : List Price -> List Price
consolidatePrices prices =
    List.foldl
        (\( name, qty ) ledger ->
            Dict.update name (\x -> Maybe.withDefault 0 x + qty |> Just) ledger
        )
        Dict.empty
        prices
        |> Dict.toList


findPriceOfFuel : Reactions -> Maybe Int
findPriceOfFuel reactions =
    expandPrice reactions ( "FUEL", 1 )
        |> Maybe.andThen (expandPrices reactions)
        |> Maybe.map (List.map Tuple.second >> List.sum)



-- Parser


priceParser : Parser Price
priceParser =
    Parser.succeed (flip Tuple.pair)
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
        >> Result.withDefault Dict.empty
