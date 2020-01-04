module Jupiter.Moons exposing
    ( calculateTotalEnergy
    , makeAxes
    , makeMoons
    , nSteps
    , repetitionAxiswise
    , step
    , tuplesToMoons
    , whenDoesRepeat
    )


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


tuplesToMoons : List ( Int, Int, Int ) -> List Moon
tuplesToMoons =
    List.map (\( x, y, z ) -> { x = x, y = y, z = z })
        >> makeMoons


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


whenDoesRepeat : List Moon -> Int
whenDoesRepeat moons =
    let
        matchState state count current =
            if current == state then
                count

            else
                matchState state (count + 1) (step current)
    in
    matchState moons 1 (step moons)


type alias Axis =
    List
        { position : Int
        , velocity : Int
        , id : Int
        }


type alias Axes =
    { x : Axis
    , y : Axis
    , z : Axis
    }


makeAxes : List Vector -> Axes
makeAxes moons =
    let
        extractAxis extractor i moon =
            { position = extractor moon
            , velocity = 0
            , id = i
            }
    in
    { x = List.indexedMap (extractAxis .x) moons
    , y = List.indexedMap (extractAxis .y) moons
    , z = List.indexedMap (extractAxis .z) moons
    }


applyGravityInAxis : Axis -> Axis
applyGravityInAxis axis =
    List.map
        (\val ->
            { val
                | velocity =
                    List.foldl
                        (\v total -> total + calculatePull v.position val.position)
                        val.velocity
                        axis
            }
        )
        axis


applyVelocityInAxis : Axis -> Axis
applyVelocityInAxis =
    List.map (\val -> { val | position = val.position + val.velocity })


stepAxis : Axis -> Axis
stepAxis =
    applyGravityInAxis >> applyVelocityInAxis


repetitionAxiswise : Axes -> Int
repetitionAxiswise axes =
    let
        nthStep axis n =
            if n <= 0 then
                axis

            else
                nthStep (stepAxis axis) (n - 1)

        checkStep ({ original, x, y, z, lastY, lastZ } as params) n =
            let
                nextX =
                    stepAxis x

                nextParams =
                    { params | x = nextX }
            in
            if nextX == original.x then
                let
                    nextY =
                        nthStep y (n - lastY)
                in
                if nextY == original.y then
                    let
                        nextZ =
                            nthStep z (n - lastZ)
                    in
                    if nextZ == original.z then
                        n

                    else
                        checkStep
                            { nextParams | y = nextY, z = nextZ, lastY = n, lastZ = n }
                            (n + 1)

                else
                    checkStep { nextParams | y = nextY, lastY = n } (n + 1)

            else
                checkStep nextParams (n + 1)
    in
    checkStep { x = axes.x, y = axes.y, z = axes.z, original = axes, lastY = 0, lastZ = 0 } 1
