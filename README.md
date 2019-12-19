# Advent

[Advent of Code](https://adventofcode.com/) is an advent calendar of coding challenges. I was feeling particularly festive, so I used this as an opportunity to learn Elf – I mean... Elm.

My first dive into pure functional programming since obligatory uni Haskell 10 years ago.

## Structure

These modules are meant to be loaded into the Elm REPL. I should document which function is intended to be run to solve each problem... but it's generally somewhat clear from what the modules expose.

- **Day 1: Tyranny of the Rocket Equation** _I hadn't decided to learn Elm at this point so it's not here_
- **Day 2: 1202 Program Alarm** `src/Intcodes/` using `Intcodes.Program.program` (`Intcodes.Solver` is for part 2, but is broken after Day 5 refactor)
- **Day 3: Crossed Wires** `src/Wires/`
- **Day 4: Secure Container** `src/Password/`
- **Day 5: Sunny with a Chance of Asteroids** `src/Intcodes/` using `Intcodes.Program.test`
