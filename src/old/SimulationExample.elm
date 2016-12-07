module SimulationExample exposing (..)

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import List exposing (..)
import Dict exposing (..)
import Time exposing (..)
import Simulation exposing (..)


createMachineWithSensorsAndSwitch : Id -> Vec2 -> Id -> List Device
createMachineWithSensorsAndSwitch startId position sensorTargetId =
    [ Machine
        { id = startId
        , position = position
        , speed =
            if (startId == 200) then
                0.4
            else if (startId == 300) then
                1.0
            else if (startId == 100) then
                0.3
            else
                0.0
        , produced = 0.0
        }
    , TemperatureSensor
        { id = startId + 1
        , position = Vec2.add position (vec2 0 30)
        , machineId = startId
        , temperature = 0
        , error = ""
        , sendInterval = Time.second * 1.5
        , timeToNextSend = Time.second * 0.1
        , targetId = sensorTargetId
        , portId = startId + 2
        }
    , Port { id = startId + 2, position = Vec2.add position (vec2 57.05 45.5), cableId = 0, receiverId = startId + 1 }
    ]
        ++ (createSwitch (startId + 10) (Vec2.add position (vec2 40 80)))



--        ++ [ Cable { id = startId + 20, port0 = startId + 2, port1 = startId + 13, packages = [] }
--   ]


createSwitch : Id -> Vec2 -> List Device
createSwitch startId position =
    [ Switch
        { id = startId
        , position = position
        , ports = [ startId + 1, startId + 2, startId + 3, startId + 4 ]
        }
    , Port { id = startId + 1, position = Vec2.add position (vec2 -10 -11), cableId = 0, receiverId = startId }
    , Port { id = startId + 2, position = Vec2.add position (vec2 0 -11), cableId = 0, receiverId = startId }
    , Port { id = startId + 3, position = Vec2.add position (vec2 10 -11), cableId = 0, receiverId = startId }
    , Port { id = startId + 4, position = Vec2.add position (vec2 0 11), cableId = 0, receiverId = startId }
    ]


createRouter : Id -> Vec2 -> List Device
createRouter startId position =
    [ Router
        { id = startId
        , position = position
        , sendInterval = Time.second * 0.2
        , packageQueue = []
        , table = Dict.empty
        , timeToNextSend = Time.second * 0.1
        , ports = [ startId + 1, startId + 2, startId + 3, startId + 4, startId + 5 ]
        , outgoingPortId = startId + 5
        }
    , Port { id = startId + 1, position = Vec2.add position (vec2 -10 0), cableId = 0, receiverId = startId }
    , Port { id = startId + 2, position = Vec2.add position (vec2 -5 0), cableId = 0, receiverId = startId }
    , Port { id = startId + 3, position = Vec2.add position (vec2 0 0), cableId = 0, receiverId = startId }
    , Port { id = startId + 4, position = Vec2.add position (vec2 5 0), cableId = 0, receiverId = startId }
    , Port { id = startId + 5, position = Vec2.add position (vec2 10 0), cableId = 0, receiverId = startId }
    ]


machineSimulation : Simulation
machineSimulation =
    Simulation.fromList
        ((createMachineWithSensorsAndSwitch 100 (vec2 100 100) 6000)
            ++ (createMachineWithSensorsAndSwitch 200 (vec2 200 100) 6000)
            ++ (createMachineWithSensorsAndSwitch 300 (vec2 300 100) 6000)
            ++ (createMachineWithSensorsAndSwitch 400 (vec2 20 250) 6000)
            ++ (createMachineWithSensorsAndSwitch 500 (vec2 80 200) 6000)
            ++ (createMachineWithSensorsAndSwitch 600 (vec2 20 550) 6000)
            ++ (createMachineWithSensorsAndSwitch 700 (vec2 80 500) 6000)
            ++ (createMachineWithSensorsAndSwitch 800 (vec2 180 500) 6000)
            ++ (createRouter 1000 (vec2 240 280))
            ++ (createRouter 2000 (vec2 140 380))
            ++ (createRouter 2100 (vec2 280 380))
            ++ (createRouter 2200 (vec2 340 480))
            ++ (createRouter 2300 (vec2 540 480))
            ++ (createRouter 2400 (vec2 280 680))
            ++ [ (FloatsVisualizer { id = 6000, position = vec2 540 200, values = Dict.empty }) ]
            ++ [ Port { id = 6001, position = vec2 540 210, cableId = 0, receiverId = 6000 } ]
        )


simulation1 : Simulation
simulation1 =
    --List.foldl (\sourceP)
    List.foldl (\( cableId, port0Id, port1Id, speed ) sim -> createCable cableId port0Id port1Id speed sim)
        machineSimulation
        [ ( 120, 102, 113, 0.1 )
        , ( 220, 202, 213, 0.1 )
        , ( 320, 302, 313, 0.1 )
        , ( 420, 402, 413, 0.1 )
        , ( 421, 414, 2001, 1.0 )
        , ( 422, 502, 513, 0.1 )
        , ( 423, 514, 2003, 0.19 )
        , ( 1101, 114, 1001, 1.0 )
        , ( 1102, 214, 1002, 1.0 )
        , ( 1103, 314, 1003, 1.0 )
          --, ( 1104, 1004, 2002, 1.0 )
        , ( 1105, 1005, 2102, 1.0 )
          --, ( 1106, 2004, 2201, 1.0 )
        , ( 1107, 2005, 2103, 1.0 )
        , ( 1108, 2105, 2202, 1.0 )
        , ( 1109, 2205, 2301, 1.0 )
        , ( 1110, 2305, 6001, 1.0 )
        , ( 1111, 2405, 2203, 1.0 )
        , ( 1112, 614, 2401, 2.0 )
        , ( 1113, 714, 2402, 2.0 )
        , ( 1114, 602, 613, 2.0 )
        , ( 1115, 702, 713, 2.0 )
        , ( 1116, 814, 2403, 2.0 )
        , ( 1117, 802, 813, 2.0 )
        ]
