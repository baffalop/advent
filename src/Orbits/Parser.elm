module Orbits.Parser exposing (parse)

import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Trailing(..)
        , chompWhile
        , sequence
        , spaces
        , succeed
        , symbol
        , variable
        )
import Set


type alias Mapping =
    ( String, String )


body : Parser String
body =
    variable
        { start = Char.isAlphaNum
        , inner = Char.isAlphaNum
        , reserved = Set.empty
        }


lineSpaces : Parser ()
lineSpaces =
    chompWhile (\c -> c == ' ' || c == '\t')


orbit : Parser Mapping
orbit =
    succeed Tuple.pair
        |= body
        |. lineSpaces
        |. symbol ")"
        |. lineSpaces
        |= body


map : Parser (List Mapping)
map =
    succeed identity
        |. spaces
        |= sequence
            { start = ""
            , end = ""
            , separator = "\n"
            , spaces = lineSpaces
            , item = orbit
            , trailing = Optional
            }


parse : String -> Result (List Parser.DeadEnd) (List Mapping)
parse =
    Parser.run map
