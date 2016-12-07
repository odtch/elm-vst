module SimulationTypes exposing (..)

import Dict exposing (Dict)
import List exposing (..)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Time exposing (Time)
import Task


type Msg
    = --Animate Time
      CreateSampleMachine Id Vec2
    | SetMachineSpeed Id Float
      --MachineMsg Id Machine.Msg
    | SendPackage Id Id PackageData



--SensorChanged Float


msgToCmd : msg -> Cmd msg
msgToCmd msg =
    Task.perform identity (Task.succeed msg)


type alias Id =
    Int


type alias Simulation =
    { devices : Dict Id Device
    , errors : List String
    }


type DeviceType
    = MachineType Machine
    | SensorType Sensor
    | PortType Port


type alias Device =
    { id : Id
    , type_ : DeviceType
    }


type alias Machine =
    { position : Vec2
    , speed : Float
    , produced : Float
    , temperature : Float
    }


type alias SensorData =
    Float


type alias Sensor =
    { getter : Simulation -> Float
    , lastValue : Float
    , sendInterval : Time
    , timeToNextSend : Time
    , portId : Id
    , targetId : Id
    }


type alias Port =
    { receiverId : Id
    , cableId : Id
    }


type alias Cable =
    { port0 : Id
    , port1 : Id
    , length : Float
    , speed : Float
    , packages : List Package
    }


type alias Package =
    { senderId : Id
    , senderPortId : Id
    , positionOnCable : Float
    , position : Vec2
    , targetId : Id
    , targetPortId : Id
    , color : String
    , glow : Float
    , data : PackageData
    }


type PackageData
    = PackageDataFindTarget Id
    | PackageDataWithFloatValue Float
    | PackageDataSensor SensorData
