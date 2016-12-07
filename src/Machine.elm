module Machine exposing (..)

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Time exposing (Time)
import SimulationTypes exposing (..)


-- type Msg
--     = SetSpeed Float
--     | Animate Time


create : Vec2 -> Machine
create position =
    { position = position
    , speed = 0.0
    , produced = 0.0
    , temperature = 20.0
    }



--
-- update : Msg -> Machine -> ( Machine, Cmd Msg )
-- update msg machine =
--     case msg of
--         SetSpeed speed ->
--             if (speed < 0 || 1.5 < speed) then
--                 Debug.crash "Machine.SetSpeed out of range"
--             else
--                 { machine | speed = speed } ! []
--
--         Animate time ->
--             let
--                 soll_temperature =
--                     (20.0 + (machine.speed * 70.0))
--
--                 temperature_factor =
--                     min 1.0 (0.1 * (Time.inSeconds time))
--
--                 new_temperature =
--                     (machine.temperature * (1.0 - temperature_factor)) + (soll_temperature * temperature_factor)
--
--                 production_rate =
--                     1.0 / (Time.second * 2.0)
--             in
--                 { machine
--                     | produced = machine.produced + (production_rate * machine.speed * time)
--                     , temperature = new_temperature
--                 }
--                     ! []


setSpeed : Float -> Machine -> Machine
setSpeed speed machine =
    { machine | speed = speed }


tick : Time -> Simulation -> Machine -> ( Machine, Cmd Msg )
tick time simulation machine =
    let
        soll_temperature =
            (20.0 + (machine.speed * 70.0))

        temperature_factor =
            min 1.0 (0.1 * (Time.inSeconds time))

        new_temperature =
            (machine.temperature * (1.0 - temperature_factor)) + (soll_temperature * temperature_factor)

        production_rate =
            1.0 / (Time.second * 2.0)
    in
        { machine
            | produced = machine.produced + (production_rate * machine.speed * time)
            , temperature = new_temperature
        }
            ! []
