module Network exposing (..)

import Dict exposing (Dict)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Time exposing (Time)
import Task
import SimulationTypes exposing (..)


type alias PortXXX =
    { receiverId : Id
    , cableId : Id
    }
