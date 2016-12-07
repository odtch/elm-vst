module Simulation exposing (..)

import Dict exposing (Dict)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Time exposing (Time)
import Task


type alias WithIdData data =
    { data
        | id : Id
    }


type alias WithPositionData data =
    WithIdData
        { data
            | position : Vec2
        }


type alias WithPort data =
    { data
        | port_ : Id
    }


type alias WithPorts data =
    { data
        | ports : List Id
    }


type alias PortData =
    WithPositionData
        { receiverId : Id
        , cableId : Id
        }


type alias MachineData =
    WithPositionData
        { speed : Float
        , produced : Float
        }


type alias TemperatureSensorData =
    WithPositionData
        { machineId : Id
        , temperature : Float
        , error : String
        , sendInterval : Time
        , timeToNextSend : Time
        , portId : Id
        , targetId : Id
        }


type alias FloatsVisualizerData =
    WithPositionData
        { values : Dict Id Float
        }


type alias SwitchData =
    WithPorts (WithPositionData {})


type alias RouterTableEntry =
    { targetPortId : Id
    , searchDepth : Int
    , timeToNextSearch : Time
    }


type alias RouterData =
    WithPorts
        (WithPositionData
            { packageQueue : List Package
            , sendInterval : Time
            , timeToNextSend : Time
            , table : Dict Id RouterTableEntry
            , outgoingPortId : Id
            }
        )


type alias CableData =
    WithIdData
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
    , type_ : PackageType
    }


type PackageType
    = PackageTypeFindTarget Id
    | PackageTypeWithFloatValue Float


type alias Id =
    Int


type Device
    = Machine MachineData
    | TemperatureSensor TemperatureSensorData
    | Switch SwitchData
    | Router RouterData
    | Cable CableData
    | Port PortData
    | FloatsVisualizer FloatsVisualizerData


type Msg
    = NoOp
    | SendPackage Package
    | ReceivePackage Package


type alias Simulation =
    { devices : Dict Id Device }


empty : Simulation
empty =
    { devices = Dict.empty
    }


fromList : List Device -> Simulation
fromList devices =
    { devices =
        --Debug.log "devs" <|
        Dict.fromList <|
            --Debug.log "d2"
            (List.map deviceToIdDeviceTupple devices)
    }


mapDevices : (Device -> a) -> Simulation -> List a
mapDevices mapper simulation =
    List.map mapper (getDevices simulation)


getDevices : Simulation -> List Device
getDevices simulation =
    Dict.values simulation.devices


getDevice : Id -> Simulation -> Device
getDevice id simulation =
    case Dict.get id simulation.devices of
        Nothing ->
            Debug.crash ("device not found " ++ (toString id))

        Just reference ->
            reference


getConnectedPorts : Simulation -> List Id -> List PortData
getConnectedPorts simulation portIds =
    List.filter (\port_ -> not (port_.cableId == 0)) <|
        List.map (\portId -> getPort portId simulation) <|
            portIds


getPort : Id -> Simulation -> PortData
getPort id simulation =
    case (getDevice id simulation) of
        Port port_ ->
            port_

        _ ->
            Debug.crash "device is not a port"


getCable : Id -> Simulation -> CableData
getCable id simulation =
    case (getDevice id simulation) of
        Cable cable ->
            cable

        _ ->
            Debug.crash "device is not a cable"


deviceToIdDeviceTupple : Device -> ( Id, Device )
deviceToIdDeviceTupple device =
    ( getDeviceId device, device )


getDeviceId : Device -> Id
getDeviceId device =
    case device of
        Machine machine ->
            machine.id

        TemperatureSensor sensor ->
            sensor.id

        Switch switch ->
            switch.id

        Router router ->
            router.id

        Cable cable ->
            cable.id

        Port port_ ->
            port_.id

        FloatsVisualizer v ->
            v.id


simulate : Time -> Simulation -> ( Simulation, Cmd Msg )
simulate time simulation =
    let
        new_devices_whith_cmd =
            Dict.map (\id device -> simulateDevice time simulation device) simulation.devices

        new_devices =
            Dict.map (\id ( device, cmd ) -> device) new_devices_whith_cmd

        new_cmds =
            Dict.values <| Dict.map (\id ( device, cmd ) -> cmd) new_devices_whith_cmd
    in
        { simulation
            | devices =
                new_devices
                -- Dict.map (\id device -> simulateDevice time simulation device) simulation.devices
        }
            ! (new_cmds)


simulateDevice : Time -> Simulation -> Device -> ( Device, Cmd Msg )
simulateDevice time simulation device =
    case device of
        Machine machine ->
            Machine { machine | produced = machine.produced + machine.speed * time } ! []

        TemperatureSensor sensor ->
            simulateTemperatureSensor time simulation sensor

        Switch switch ->
            Switch switch ! []

        Router router ->
            simulateRouter time simulation router

        Cable cable ->
            simulateCable time
                simulation
                cable

        Port port_ ->
            Port port_ ! []

        FloatsVisualizer v ->
            FloatsVisualizer v ! []


getRouterTableEntry : Id -> RouterData -> ( RouterTableEntry, RouterData )
getRouterTableEntry targetId router =
    case Dict.get targetId router.table of
        Nothing ->
            let
                entry =
                    { targetPortId = 0, searchDepth = 0, timeToNextSearch = 0 }
            in
                ( entry, { router | table = Dict.insert targetId entry router.table } )

        Just entry ->
            ( entry, router )


simulateRouterNextPackage : Time -> Simulation -> Package -> RouterData -> ( Device, Cmd Msg )
simulateRouterNextPackage time simulation package router =
    -- case Dict.get package.targetId router.table of
    --     Nothing ->
    --         Debug.crash "router has no entry to this target"
    --
    --     Just entry ->
    (Router router)
        ! [ msgToCmd
                (SendPackage
                    { package
                        | --senderId = router.id
                          --,
                          senderPortId = router.outgoingPortId
                    }
                )
          ]


simulateRouter : Time -> Simulation -> RouterData -> ( Device, Cmd Msg )
simulateRouter time simulation router =
    let
        newTimeToNextSend =
            router.timeToNextSend - time
    in
        if (newTimeToNextSend < 0) then
            case List.head router.packageQueue of
                Nothing ->
                    (Router { router | timeToNextSend = 0 }) ! []

                Just package ->
                    simulateRouterNextPackage time
                        simulation
                        package
                        { router
                            | timeToNextSend = newTimeToNextSend + router.sendInterval
                            , packageQueue = List.drop 1 router.packageQueue
                        }
        else
            (Router { router | timeToNextSend = newTimeToNextSend }) ! []


simulatePackageOnCable : Time -> Simulation -> CableData -> Package -> Package
simulatePackageOnCable time simulation cable package =
    let
        ( startPortId, endPortId, dir ) =
            if (package.senderPortId == cable.port0) then
                ( cable.port0, cable.port1, 1.0 )
            else if (package.senderPortId == cable.port1) then
                ( cable.port1, cable.port0, 1.0 )
            else
                Debug.crash "package.senderPortId not on cable"

        startPos =
            (getPort startPortId simulation).position

        endPos =
            (getPort endPortId simulation).position

        startPosToEndPos =
            Vec2.sub endPos startPos

        newPosOnCable =
            package.positionOnCable + time * dir * cable.speed * 0.1

        newPos =
            Vec2.add startPos (Vec2.scale (newPosOnCable / cable.length) startPosToEndPos)
    in
        --Debug.log "p"
        { package | positionOnCable = newPosOnCable, position = newPos }


simulateCable : Time -> Simulation -> CableData -> ( Device, Cmd Msg )
simulateCable time simulation cable =
    let
        new_packages =
            List.map
                (\package ->
                    simulatePackageOnCable time simulation cable package
                )
                cable.packages

        pend_packages =
            List.filter (\p -> p.positionOnCable < cable.length) new_packages

        arr_packages =
            List.filter (\p -> not (p.positionOnCable < cable.length)) new_packages

        -- type alias CableData =
        --     WithIdData
        --         { port0 : Id
        --         , port1 : Id
        --         , packages : List Package
    in
        Cable { cable | packages = pend_packages }
            ! (List.map
                (\p ->
                    (msgToCmd
                        (ReceivePackage p)
                    )
                )
                arr_packages
              )


simulateTemperatureSensor : Time -> Simulation -> TemperatureSensorData -> ( Device, Cmd Msg )
simulateTemperatureSensor time simulation sensor =
    let
        f =
            min 1.0 (0.51 * (Time.inSeconds time))

        new_sensor =
            case Dict.get sensor.machineId simulation.devices of
                Nothing ->
                    { sensor | error = "machine ot found" }

                Just reference ->
                    case reference of
                        Machine machine ->
                            { sensor
                                | temperature = (sensor.temperature * (1.0 - f)) + (f * (20.0 + (machine.speed * 70.0)))
                                , timeToNextSend = sensor.timeToNextSend - time
                            }

                        _ ->
                            { sensor | error = "reference is not a machine" }
    in
        --Debug.log "sise" <|
        if (new_sensor.timeToNextSend < 0) then
            TemperatureSensor { new_sensor | timeToNextSend = new_sensor.timeToNextSend + new_sensor.sendInterval }
                ! [ (msgToCmd
                        (SendPackage
                            { senderId = sensor.id
                            , senderPortId = sensor.portId
                            , positionOnCable = 0
                            , position = vec2 0 0
                            , targetId = sensor.targetId
                            , targetPortId = 0
                            , color = temperatureColor new_sensor.temperature
                            , glow = 0.5
                            , type_ = PackageTypeWithFloatValue new_sensor.temperature
                            }
                        )
                    )
                  ]
        else
            TemperatureSensor new_sensor ! []


{-| A command to generate a message without performing any action.
This is useful for implementing components that generate events in the manner
of HTML elements, but where the event fires from within Elm code, rather than
by an external trigger.
-}
msgToCmd : msg -> Cmd msg
msgToCmd x =
    --Task.perform identity identity (Task.succeed x)
    Task.perform identity (Task.succeed x)


update : Msg -> Simulation -> ( Simulation, Cmd Msg )
update msg simulation =
    case msg of
        NoOp ->
            simulation ! []

        SendPackage package ->
            sendPackage package simulation

        ReceivePackage package ->
            receivePackage package simulation


opositePortIdOnCable : Id -> CableData -> Id
opositePortIdOnCable portId cable =
    if (cable.port0 == portId) then
        cable.port1
    else if (cable.port1 == portId) then
        cable.port0
    else
        Debug.crash "port not on cable"


sendPackage : Package -> Simulation -> ( Simulation, Cmd Msg )
sendPackage package simulation =
    let
        senderPort =
            --Debug.log "senderPort" <|
            getPort package.senderPortId simulation

        cable =
            getCable senderPort.cableId simulation

        targetPortId =
            opositePortIdOnCable senderPort.id cable

        new_package =
            { package | position = senderPort.position, positionOnCable = 0, targetPortId = targetPortId }

        new_cable =
            { cable | id = cable.id, packages = new_package :: cable.packages }
    in
        { simulation
            | devices = Dict.insert new_cable.id (Cable new_cable) simulation.devices
        }
            ! []


temperatureColor : Float -> String
temperatureColor temperature =
    if (temperature < 40) then
        "#00ff00"
    else if (temperature < 55) then
        "#ffc600"
    else
        "#ff0000"


receivePackage : Package -> Simulation -> ( Simulation, Cmd Msg )
receivePackage package simulation =
    let
        receiverPort =
            getPort package.targetPortId simulation

        receiver =
            getDevice receiverPort.receiverId simulation
    in
        case receiver of
            TemperatureSensor sensor ->
                simulation ! []

            FloatsVisualizer v ->
                case package.type_ of
                    PackageTypeWithFloatValue val ->
                        let
                            nv =
                                { v | values = Dict.insert package.senderId val v.values }
                        in
                            ({ simulation | devices = Dict.insert nv.id (FloatsVisualizer nv) simulation.devices }) ! []

                    _ ->
                        Debug.crash "invalid package type for fv"

            Router router ->
                let
                    newrouter =
                        { router | packageQueue = package :: router.packageQueue }
                in
                    ({ simulation | devices = Dict.insert newrouter.id (Router newrouter) simulation.devices })
                        ! []

            Switch switch ->
                --Debug.log "receivePackage receiver not handled" <|
                let
                    redirectPorts =
                        getConnectedPorts simulation <|
                            List.filter (\portId -> not (portId == receiverPort.id))
                                switch.ports

                    redirectCmds =
                        List.map
                            (\redirectPort ->
                                (msgToCmd
                                    (SendPackage
                                        { package
                                            | senderPortId = redirectPort.id
                                            , positionOnCable = 0
                                            , position = vec2 0 0
                                        }
                                    )
                                )
                            )
                            redirectPorts
                in
                    simulation
                        ! redirectCmds

            _ ->
                Debug.log "receivePackage receiver not handled" <| simulation ! []


createCable : Id -> Id -> Id -> Float -> Simulation -> Simulation
createCable cableId port0Id port1Id speed simulation =
    let
        port0 =
            getPort port0Id simulation

        port1 =
            getPort port1Id simulation

        cable =
            { id = cableId
            , port0 = port0.id
            , port1 = port1.id
            , length = Vec2.length (Vec2.sub port0.position port1.position)
            , speed = speed
            , packages = []
            }
    in
        if (not (port0.cableId == 0)) then
            Debug.crash "port has already a cable"
        else if (not (port1.cableId == 0)) then
            Debug.crash "port has already a cable"
        else
            { simulation
                | devices =
                    Dict.insert port0.id
                        (Port { port0 | cableId = cableId })
                    <|
                        Dict.insert port1.id
                            (Port { port1 | cableId = cableId })
                        <|
                            Dict.insert
                                cable.id
                                (Cable cable)
                                simulation.devices
            }



-- , portId : Id
-- , targetId : Id
-- { senderId : Id
-- , senderPortId : Id
-- , targetId : Id
-- , data : PackageData
