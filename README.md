# Advent

[Advent of Code](https://adventofcode.com/) is an advent calendar of coding challenges. I was feeling particularly festive, so I used this as an opportunity to learn Elf – I mean... Elm.

My first dive into pure functional programming since obligatory uni Haskell 10 years ago.

## Structure

These modules are meant to be loaded into the Elm REPL. I should document which function is intended to be run to solve each problem... but it's generally somewhat clear from what the modules expose.

- **Day 1: Tyranny of the Rocket Equation** _I hadn't decided to learn Elm at this point so it's not here_
- **Day 2: 1202 Program Alarm** `src/Intcodes/` using `Intcodes.Program.program` as input
  - `Intcodes.Solver` is for part 2, but is broken after Day 5 refactor
- **Day 3: Crossed Wires** `src/Wires/`
- **Day 4: Secure Container** `src/Password/`
- **Day 5: Sunny with a Chance of Asteroids** `src/Intcodes/` using `Intcodes.Program.test` as input
- **Day 6: Universal Orbit Map** `src/Orbits/`
- **Day 7: Amplification Circuit** `src/Amps/`
- **Day 8: Space Image Format** `src/Image/`
- **Day 9: Sensor Boost** `src/Intcodes` using `Intcodes.Program.boost`
- **Day 10: Monitoring Station** `src/Asteroids`
- **Day 11: Space Police** `src/Painting`
- **Day 12: The N-Body Problem** `src/Jupiter`
- **Day 13: Care Package** `src/Arcade`
    - For part 2, `elm make src/Arcade/Main.elm` makes the browser-based game
- **Day 14: Space Stoichiometry** `src/Factory`
- **Day 15: Oxygen System** `src/Oxygen`
- **Day 16: Flawed Frequency Transmission** `src/FFT`
