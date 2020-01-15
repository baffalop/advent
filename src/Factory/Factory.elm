module Factory.Factory exposing (Reactions, accountForFuel, findPriceOfFuel, howMuchCanIProduce, parse)

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

                    difference =
                        requiredQty - (multiple * acquiredQty)

                    priceListOut =
                        List.map (Tuple.mapSecond ((*) multiple)) priceList
                in
                if difference == 0 then
                    priceListOut

                else
                    ( name, difference ) :: priceListOut
            )


expandPrices : Reactions -> Dict String Int -> Maybe (Dict String Int)
expandPrices reactions prices =
    case prices |> Dict.toList |> getFirstUnexpandedPrice of
        Nothing ->
            Just prices

        Just price ->
            let
                rest =
                    Dict.remove (Tuple.first price) prices
            in
            Maybe.map (addPrices rest) (expandPrice reactions price)
                |> Maybe.andThen (expandPrices reactions)


getFirstUnexpandedPrice : List Price -> Maybe Price
getFirstUnexpandedPrice prices =
    case prices of
        [] ->
            Nothing

        ( name, qty ) :: rest ->
            if name == "ORE" || qty <= 0 then
                getFirstUnexpandedPrice rest

            else
                Just ( name, qty )


addPrices : Dict String Int -> List Price -> Dict String Int
addPrices ledger prices =
    List.foldl
        (\( name, qty ) ledger_ ->
            Dict.update name (\x -> Maybe.withDefault 0 x + qty |> Just) ledger_
        )
        ledger
        prices


accountForFuel : Reactions -> Maybe (Dict String Int)
accountForFuel reactions =
    expandPrice reactions ( "FUEL", 1 )
        |> Maybe.map Dict.fromList
        |> Maybe.andThen (expandPrices reactions)


findPriceOfFuel : Reactions -> Maybe Int
findPriceOfFuel reactions =
    accountForFuel reactions
        |> Maybe.andThen (Dict.get "ORE")


howMuchCanIProduce : Reactions -> Int -> Int
howMuchCanIProduce reactions ore =
    let
        countdown count prices =
            if (Dict.get "ORE" prices |> Maybe.withDefault 0) > ore then
                count

            else
                let
                    nextExpansion =
                        Dict.insert "FUEL" 1 prices
                            |> expandPrices reactions
                            |> Maybe.withDefault Dict.empty
                in
                countdown (count + 1) nextExpansion
    in
    expandPrice reactions ( "FUEL", 1 )
        |> Maybe.map Dict.fromList
        |> Maybe.andThen (expandPrices reactions)
        |> Maybe.withDefault Dict.empty
        |> countdown 0



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
