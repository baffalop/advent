module Orbits.Parser exposing (map, parse)

import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Trailing(..)
        , chompWhile
        , sequence
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
    sequence
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
