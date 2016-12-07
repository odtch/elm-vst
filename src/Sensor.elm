module Sensor exposing (..)

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Time exposing (..)
import SimulationTypes exposing (..)
import Simulation exposing (..)


create : (Simulation -> Float) -> Id -> Id -> Sensor
create getter portId targetId =
    { getter = getter
    , lastValue = 0
    , sendInterval = second * 1
    , timeToNextSend = second * 3
    , portId = portId
    , targetId = targetId
    }


tick : Time -> Simulation -> Sensor -> ( Sensor, Cmd Msg )
tick time simulation sensor =
    let
        newTimeToNextSend =
            sensor.timeToNextSend - time
    in
        if (0 < newTimeToNextSend) then
            { sensor | timeToNextSend = newTimeToNextSend } ! []
        else
            let
                newValue =
                    sensor.getter simulation
            in
                ( { sensor
                    | timeToNextSend = newTimeToNextSend + sensor.sendInterval
                    , lastValue = newValue
                  }
                , msgToCmd (SendPackage sensor.portId sensor.targetId (PackageDataSensor newValue))
                )
