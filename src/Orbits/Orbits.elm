module Orbits.Orbits exposing (addOrbit, countOrbits, initCOM)

import Dict exposing (Dict)


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
