module Orbits.Orbits exposing (addOrbit, countOrbits, initCOM, parseMap)

import Dict exposing (Dict)
import Orbits.Parser as Parser


type Body
    = COM
        { name : String
        , moons : Dict String Body
        }
    | InOrbit
        { orbitCount : Int
        , moons : Dict String Body
        }


initCOM : String -> Result String Body
initCOM name =
    Ok
        (COM
            { name = name
            , moons = Dict.empty
            }
        )


addOrbit : String -> String -> Result String Body -> Result String Body
addOrbit nodeName moonName =
    Result.andThen <|
        \root ->
            let
                newRoot =
                    replaceNode nodeName (addMoonToNode moonName) root
            in
            if newRoot == root then
                Err ("Couldn't find node " ++ nodeName ++ " in " ++ nodeName ++ ")" ++ moonName)

            else
                Ok newRoot


getOrbitCount : Body -> Int
getOrbitCount body =
    case body of
        COM _ ->
            0

        InOrbit { orbitCount } ->
            orbitCount


addMoonToNode : String -> Body -> Body
addMoonToNode name node =
    let
        moon =
            InOrbit
                { orbitCount = getOrbitCount node + 1
                , moons = Dict.empty
                }

        addMoon =
            \body -> { body | moons = Dict.insert name moon body.moons }
    in
    case node of
        COM body ->
            COM (addMoon body)

        InOrbit body ->
            InOrbit (addMoon body)


replaceNode : String -> (Body -> Body) -> Body -> Body
replaceNode nodeName nodeUpdate root =
    let
        updateMoons =
            \body ->
                if Dict.member nodeName body.moons then
                    { body
                        | moons = Dict.update nodeName (Maybe.map nodeUpdate) body.moons
                    }

                else
                    { body
                        | moons = Dict.map (always <| replaceNode nodeName nodeUpdate) body.moons
                    }
    in
    case root of
        COM body ->
            if nodeName == body.name then
                nodeUpdate root

            else
                COM (updateMoons body)

        InOrbit body ->
            InOrbit (updateMoons body)


parseMap : String -> Result String Body
parseMap input =
    case Parser.parse input of
        Err deadEnds ->
            case deadEnds of
                [] ->
                    Err "Why would the parser output empty DeadEnds?"

                { col, row } :: _ ->
                    Err
                        ("Error parsing at col "
                            ++ String.fromInt col
                            ++ ": row "
                            ++ String.fromInt row
                        )

        Ok list ->
            case list of
                [] ->
                    Err "Parsed input was empty"

                ( com, first ) :: rest ->
                    List.foldl
                        (\( body, moon ) -> addOrbit body moon)
                        (initCOM com |> addOrbit com first)
                        rest


countOrbits : Body -> Int
countOrbits root =
    let
        sumOrbits =
            Dict.foldl (\_ moon sum -> sum + countOrbits moon) 0
    in
    case root of
        COM { moons } ->
            sumOrbits moons

        InOrbit { orbitCount, moons } ->
            orbitCount + sumOrbits moons
