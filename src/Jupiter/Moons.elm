module Jupiter.Moons exposing (calculateTotalEnergy, makeMoons, nSteps, step)


type alias Vector =
    { x : Int
    , y : Int
    , z : Int
    }


type alias Moon =
    { position : Vector
    , velocity : Vector
    }


stationary : Vector
stationary =
    { x = 0, y = 0, z = 0 }


makeMoons : List Vector -> List Moon
makeMoons =
    List.map
        (\position ->
            { position = position
            , velocity = stationary
            }
        )


mapVectors : (Int -> Int -> Int) -> Vector -> Vector -> Vector
mapVectors f v1 v2 =
    { x = f v1.x v2.x
    , y = f v1.y v2.y
    , z = f v1.z v2.z
    }


addVectors : Vector -> Vector -> Vector
addVectors v1 v2 =
    { x = v1.x + v2.x
    , y = v1.y + v2.y
    , z = v1.z + v2.z
    }


calculatePull : Int -> Int -> Int
calculatePull influencer influenced =
    if influenced < influencer then
        1

    else if influenced > influencer then
        -1

    else
        0


applyGravity : Moon -> Moon -> Moon
applyGravity influencer influenced =
    let
        change =
            mapVectors calculatePull influencer.position influenced.position
    in
    { influenced | velocity = addVectors influenced.velocity change }


applyAllGravities : List Moon -> List Moon
applyAllGravities moons =
    List.map (\moon -> List.foldl applyGravity moon moons) moons


applyVelocities : List Moon -> List Moon
applyVelocities =
    List.map
        (\moon ->
            { moon
                | position = addVectors moon.position moon.velocity
            }
        )


step : List Moon -> List Moon
step =
    applyAllGravities >> applyVelocities


nSteps : Int -> List Moon -> List Moon
nSteps n moons =
    if n <= 0 then
        moons

    else
        nSteps (n - 1) (step moons)


calculateEnergy : Vector -> Int
calculateEnergy { x, y, z } =
    abs x + abs y + abs z


calculateTotalEnergy : List Moon -> Int
calculateTotalEnergy moons =
    let
        potentialEnergies =
            List.map (.position >> calculateEnergy) moons

        kineticEnergies =
            List.map (.velocity >> calculateEnergy) moons
    in
    List.map2 (*) potentialEnergies kineticEnergies
        |> List.sum
