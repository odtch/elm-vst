module SimulationStoryExample exposing (..)

import Time exposing (..)
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


story1 =
    Story.create
        [ Story.Wait <| 1 * second
        , Story.Send <| CreateSampleMachine 2000 (vec2 2 0)
        , Story.Wait <| 2 * second
        , Story.Send <| SetMachineSpeed 2000 1.0
        , Story.Wait <| 2 * second
        , Story.Send <| SetMachineSpeed 2000 0.0
        , Story.Wait <| 1 * second
        , Story.Send <| CreateSampleMachine 3000 (vec2 3 0)
        , Story.Wait <| 2 * second
        , Story.Send <| SetMachineSpeed 2000 1.0
        , Story.Send <| SetMachineSpeed 3000 1.0
        , Story.Wait <| 2 * second
        , Story.Send <| SetMachineSpeed 2000 0.0
        , Story.Send <| SetMachineSpeed 3000 0.0
        ]
