module Orbits.Orbits exposing (addOrbit, countOrbits, initCOM)


type Body
    = COM
        { name : String
        , moons : List Body
        }
    | InOrbit
        { name : String
        , orbitCount : Int
        , moons : List Body
        }


initCOM : String -> Body
initCOM name =
    COM
        { name = name
        , moons = []
        }


addOrbit : String -> String -> Body -> Body
addOrbit nodeName moonName =
    addMoonToNode moonName
        |> replaceNode nodeName


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
                { name = name
                , orbitCount = getOrbitCount node + 1
                , moons = []
                }

        addMoon =
            \body -> { body | moons = moon :: body.moons }
    in
    case node of
        COM body ->
            COM (addMoon body)

        InOrbit body ->
            InOrbit (addMoon body)


replaceNode : String -> (Body -> Body) -> Body -> Body
replaceNode nodeName nodeUpdate root =
    let
        replaceMoons =
            \body ->
                { body
                    | moons = List.map (replaceNode nodeName nodeUpdate) body.moons
                }
    in
    case root of
        COM body ->
            if body.name == nodeName then
                nodeUpdate root

            else
                COM (replaceMoons body)

        InOrbit body ->
            if body.name == nodeName then
                nodeUpdate root

            else
                InOrbit (replaceMoons body)


countOrbits : Body -> Int
countOrbits root =
    let
        sumOrbits =
            List.foldl (\moon sum -> sum + countOrbits moon) 0
    in
    case root of
        COM { moons } ->
            sumOrbits moons

        InOrbit { orbitCount, moons } ->
            orbitCount + sumOrbits moons
