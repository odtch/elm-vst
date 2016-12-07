module SimulationTest exposing (..)

import Test exposing (..)
import Time exposing (..)
import Expect
import Dict
import Fuzz exposing (list, int, tuple, string)
import String
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Simulation exposing (..)
import SimulationTypes exposing (..)
import SimulationUpdate exposing (..)
import Machine exposing (..)
import Sensor exposing (..)


sim0 =
    empty


sim1 =
    addDevice
        1000
        (MachineType (Machine.create (vec2 0 0)))
        sim0


( sim2, msgs2 ) =
    update (SetMachineSpeed 1000 1.0) sim1



--MachineMsg 1000 (Machine.SetSpeed 1.0)) sim1


sim3 =
    addDevice
        1001
        (SensorType
            (Sensor.create
                (\simulation ->
                    case findDevice 1000 simulation of
                        Just device ->
                            case device.type_ of
                                MachineType machine ->
                                    machine.temperature

                                _ ->
                                    Debug.crash "device not a machine"

                        Nothing ->
                            Debug.crash "machine not found"
                )
                1002
                9000
            )
        )
        sim2



--
-- machine0 =
--     Machine.create (vec2 0 0)
--
--
-- ( machine1, cmds1 ) =
--     Machine.update (Machine.SetSpeed 1.0) machine0
-- ( machine2, cmds2 ) =
--     Machine.update (Machine.Animate (second * 1.0)) machine1
-- ( machine3, cmds3 ) =
--     Machine.update (Machine.Animate (second * 1.0)) machine2


all : Test
all =
    describe "Simulation Test"
        [ test "initial simulation" <|
            \() -> Expect.equal (Dict.size sim0.devices) 0
        , test "no errors 0" <|
            \() -> Expect.equal sim0.errors []
        , test "no errors 1" <|
            \() -> Expect.equal sim1.errors []
        , test "device added" <|
            \() -> Expect.equal (Dict.size sim1.devices) 1
        , test "no errors 2" <|
            \() -> Expect.equal sim2.errors []
        , test "machine speed changed" <|
            \() ->
                Expect.equal
                    (case (getDevice 1000 sim2).type_ of
                        MachineType machine ->
                            machine.speed

                        _ ->
                            -1.0
                    )
                    1.0
        , test "no errors 3" <|
            \() -> Expect.equal sim3.errors []
          -- , test "set speed" <|
          --     \() -> Expect.equal machine1.speed 1.0
          -- , test "produced" <|
          --     \() -> Expect.equal machine2.produced 0.5
          -- , test "temperature changed" <|
          --     \() -> Expect.equal machine2.temperature 27.0
          -- , test "produced 2" <|
          --     \() -> Expect.equal machine3.produced 1.0
          -- , test "temperature changed 2" <|
          --     \() -> Expect.equal machine3.temperature 33.3
        ]
