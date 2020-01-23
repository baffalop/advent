module Factory.Factory exposing (Reactions, findCostOfFuel, howMuchCanIProduce, parse)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser)
import Set
import Utils exposing (flip)


type alias Cost =
    ( String, Int )


type alias Reactions =
    Dict String ( Int, List Cost )


expandCost : Reactions -> Cost -> Maybe (List Cost)
expandCost reactions ( name, requiredQty ) =
    Dict.get name reactions
        |> Maybe.map
            (\( acquiredQty, costList ) ->
                let
                    multiple =
                        ceiling (toFloat requiredQty / toFloat acquiredQty)

                    difference =
                        requiredQty - (multiple * acquiredQty)

                    output =
                        List.map (Tuple.mapSecond ((*) multiple)) costList
                in
                if difference == 0 then
                    output

                else
                    ( name, difference ) :: output
            )


expandCosts : Reactions -> Dict String Int -> Maybe (Dict String Int)
expandCosts reactions prices =
    case prices |> Dict.toList |> getFirstUnexpandedCost of
        Nothing ->
            Just prices

        Just price ->
            let
                rest =
                    Dict.remove (Tuple.first price) prices
            in
            Maybe.map (addCosts rest) (expandCost reactions price)
                |> Maybe.andThen (expandCosts reactions)


getFirstUnexpandedCost : List Cost -> Maybe Cost
getFirstUnexpandedCost costs =
    case costs of
        [] ->
            Nothing

        ( name, qty ) :: rest ->
            if name == "ORE" || qty <= 0 then
                getFirstUnexpandedCost rest

            else
                Just ( name, qty )


addCosts : Dict String Int -> List Cost -> Dict String Int
addCosts ledger costs =
    List.foldl
        (\( name, qty ) ledger_ ->
            Dict.update name (\x -> Maybe.withDefault 0 x + qty |> Just) ledger_
        )
        ledger
        costs


accountForFuel : Reactions -> Int -> Maybe (Dict String Int)
accountForFuel reactions amount =
    expandCost reactions ( "FUEL", amount )
        |> Maybe.map Dict.fromList
        |> Maybe.andThen (expandCosts reactions)


addFuel : Reactions -> Int -> Maybe (Dict String Int) -> Maybe (Dict String Int)
addFuel reactions amount =
    Maybe.andThen
        (Dict.insert "FUEL" amount >> expandCosts reactions)


findCostOfFuel : Reactions -> Int -> Maybe Int
findCostOfFuel reactions amount =
    accountForFuel reactions amount
        |> Maybe.andThen (Dict.get "ORE")


howMuchCanIProduce : Reactions -> Int -> Int
howMuchCanIProduce reactions ore =
    let
        costOfFuel =
            findCostOfFuel reactions 1
                |> Maybe.withDefault ore

        costOfFuelAsFloat =
            toFloat costOfFuel

        howMuch currentOre outlaySoFar =
            let
                thisOutlay =
                    floor (toFloat currentOre / costOfFuelAsFloat) |> max 1

                outlay =
                    outlaySoFar + thisOutlay

                spentOre =
                    findCostOfFuel reactions outlay
                        |> Maybe.withDefault (currentOre + 1)

                remainingOre =
                    ore - spentOre
            in
            if remainingOre < 0 then
                outlaySoFar

            else
                howMuch remainingOre outlay
    in
    howMuch ore 0



-- Parser


costParser : Parser Cost
costParser =
    Parser.succeed (flip Tuple.pair)
        |= Parser.int
        |. Parser.spaces
        |= Parser.variable
            { start = Char.isAlpha
            , inner = Char.isAlpha
            , reserved = Set.empty
            }


costsParser : Parser (List Cost)
costsParser =
    Parser.sequence
        { start = ""
        , end = ""
        , separator = ","
        , spaces = Parser.spaces
        , item = costParser
        , trailing = Parser.Forbidden
        }


reactionParser : Parser { name : String, qty : Int, costs : List Cost }
reactionParser =
    Parser.succeed (\costs qty name -> { name = name, qty = qty, costs = costs })
        |= costsParser
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
                    (\{ name, qty, costs } ->
                        Parser.Loop <| Dict.insert name ( qty, costs ) reactions
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
