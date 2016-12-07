module Example exposing (..)

import Time exposing (Time)
import SimulationTypes exposing (..)
import Simulation exposing (..)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import SimulationUpdate
import SimulationTick
import Machine
import Animation
import Story
import AnimationFrame
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
