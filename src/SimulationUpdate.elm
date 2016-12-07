module SimulationUpdate exposing (..)

import Dict exposing (Dict)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Time exposing (Time)
import Task
import SimulationTypes exposing (..)
import Simulation exposing (..)
import Machine exposing (..)
import Network exposing (..)


update : Msg -> Simulation -> ( Simulation, Cmd Msg )
update msg simulation =
    case msg of
        CreateSampleMachine id pos ->
            (addDevice id
                (MachineType (Machine.create pos))
                simulation
            )
                ! []

        SetMachineSpeed id speed ->
            case findDevice id simulation of
                Just device ->
                    case device.type_ of
                        MachineType machine ->
                            (replaceDevice
                                { device
                                    | type_ = MachineType <| Machine.setSpeed speed machine
                                }
                                simulation
                            )
                                ! []

                        _ ->
                            (error "device not a machine" simulation) ! []

                Nothing ->
                    (error "machine not found" simulation) ! []

        SendPackage senderPortId targetId data ->
            (error "sendPackage not yet implemented" simulation) ! []
