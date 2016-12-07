module MachineTest exposing (..)

import Test exposing (..)
import Time exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import SimulationTypes exposing (..)
import Simulation exposing (..)
import Machine exposing (..)


machine0 =
    Machine.create (vec2 0 0)



-- ( machine1, cmds1 ) =
--     Machine.update (Machine.SetSpeed 1.0) machine0


machine1 =
    Machine.setSpeed 1.0 machine0


( machine2, cmds2 ) =
    Machine.tick (second * 1.0) Simulation.empty machine1
( machine3, cmds3 ) =
    Machine.tick (second * 1.0) Simulation.empty machine2


all : Test
all =
    describe "Machine Test"
        [ test "initial speed" <|
            \() -> Expect.equal machine0.speed 0.0
        , test "set speed" <|
            \() -> Expect.equal machine1.speed 1.0
        , test "produced" <|
            \() -> Expect.equal machine2.produced 0.5
        , test "temperature changed" <|
            \() -> Expect.equal machine2.temperature 27.0
        , test "produced 2" <|
            \() -> Expect.equal machine3.produced 1.0
        , test "temperature changed 2" <|
            \() -> Expect.equal machine3.temperature 33.3
        ]
