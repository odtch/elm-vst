module SimulationTick exposing (..)

import Dict exposing (Dict)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Time exposing (Time)
import Task
import SimulationTypes exposing (..)
import Simulation exposing (..)
import Machine exposing (..)
import Sensor exposing (..)
import Network exposing (..)


tick : Time -> Simulation -> ( Simulation, Cmd Msg )
tick time simulation =
    let
        ( newdevices, msgs ) =
            Dict.foldl
                (\id device ( devices, msgs ) ->
                    let
                        ( new_device, device_msgs ) =
                            tickDevice time simulation device
                    in
                        ( Dict.insert id new_device devices, msgs ++ [ device_msgs ] )
                )
                ( simulation.devices, [] )
                simulation.devices
    in
        { simulation | devices = newdevices } ! msgs


tickDevice : Time -> Simulation -> Device -> ( Device, Cmd Msg )
tickDevice time simulation device =
    case device.type_ of
        MachineType machine ->
            let
                ( new_machine, machine_msgs ) =
                    Machine.tick time simulation machine
            in
                ( { device | type_ = MachineType new_machine }
                , --Cmd.map (\m -> MachineMsg device.id m)
                  machine_msgs
                )

        SensorType sensor ->
            let
                ( new_sensor, sensor_msgs ) =
                    Sensor.tick time simulation sensor
            in
                ( { device | type_ = SensorType new_sensor }
                , --Cmd.map (\m -> MachineMsg device.id m)
                  sensor_msgs
                )

        PortType port_ ->
            device ! []
