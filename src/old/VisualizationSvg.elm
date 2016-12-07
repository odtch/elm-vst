module VisualizationSvg exposing (..)

import VirtualDom as VD
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import String
import Dict
import List
import Time exposing (Time)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Simulation exposing (Simulation)
import VisualizationHtml as VH
import AnimationFrame
import Svg exposing (..)
import Svg.Events as SE exposing (..)
import Svg.Attributes exposing (..)
import Simulation exposing (..)


type Msg
    = OnMachineClick Simulation.Id


update : Msg -> Simulation -> ( Simulation, Cmd Msg )
update msg simulation =
    case msg of
        OnMachineClick id ->
            (Debug.log ("macli" ++ (toString id)) simulation) ! []


translation : Vec2 -> String
translation pos =
    "translate(" ++ (toString <| Vec2.getX pos) ++ "," ++ (toString <| Vec2.getY pos) ++ ")"


vec2xky : Vec2 -> String
vec2xky pos =
    (toString <| Vec2.getX pos) ++ "," ++ (toString <| Vec2.getY pos)


viewMachinePackage : Float -> Svg.Svg Msg
viewMachinePackage production =
    let
        pf =
            ((production) - toFloat (round (production)))

        o =
            toString (1.0 * (0.5 - (1.2 * pf)))
    in
        rect
            [ height "39.96"
            , width "39.96"
            , x (toString (150.0 - 300.0 * pf))
            , y "233.112"
            , fill "#00A99D"
            , fillOpacity o
            , stroke "#000000"
            , strokeOpacity o
            , strokeWidth "15"
            , strokeMiterlimit "4"
            , strokeLinejoin "round"
            ]
            []


viewMachine : Simulation.MachineData -> Svg.Svg Msg
viewMachine machine =
    let
        production =
            machine.produced * 0.002
    in
        g
            [ transform (translation machine.position)
            ]
            [ g
                [ transform ("scale( 0.1)") ]
                [ g [ x "9000", onClick (OnMachineClick machine.id) ]
                    [ viewMachinePackage (production + 0.0)
                    , viewMachinePackage (production + 0.25)
                    , viewMachinePackage (production + 0.5)
                    , viewMachinePackage (production + 0.75)
                    , rect
                        [ height "288.484", fill "#F7931E", width "213.969", x "287.832", y "40.123" ]
                        []
                    , polygon [ points "501.801,328.596 244.781,328.596 287.83,377.314 468.368,377.314 ", fill "#00A99D" ] []
                    , rect [ height "94.567", fill "#F7931E", width "180.536", x "287.832", y "377.309" ] []
                    , rect [ height "169.072", fill "#B3B3B3", width "81.196", x "324.131", y "76.417" ] []
                    , rect [ height "39.165", fill "#F7931E", width "33.028", x "348.211", y "131.824" ] []
                    , Svg.path [ d "M37.962,328.596H287.83V273.07H37.962c-15.333,0.001-27.763,12.431-27.763,27.764l0,0 C10.199,316.167,22.629,328.596,37.962,328.596z", fill "#B3B3B3" ] []
                    , Svg.path [ d "m 512,40.118 c 0,-5.632 -4.567,-10.199 -10.199,-10.199 l -213.971,0 c -5.632,0 -10.199,4.567 -10.199,10.199 l 0,222.754 C 117.78441,262.50092 119.0417,262.873 37.962,262.873 17.03,262.872 0,279.902 0,300.834 c 0,20.932 17.03,37.962 37.962,37.962 l 202.221,0 37.448,42.378 0,90.708 c 0,5.632 4.567,10.199 10.199,10.199 l 180.537,0 c 5.632,0 10.199,-4.567 10.199,-10.199 l 0,-91.405 31.643,-46.109 c 0.005,-0.007 0.008,-0.014 0.012,-0.021 0.24,-0.35 0.452,-0.718 0.647,-1.097 0.042,-0.082 0.08,-0.164 0.118,-0.247 0.153,-0.317 0.289,-0.643 0.408,-0.977 0.027,-0.074 0.057,-0.148 0.082,-0.222 0.133,-0.399 0.244,-0.807 0.326,-1.225 0.009,-0.048 0.013,-0.096 0.021,-0.144 0.064,-0.351 0.108,-0.709 0.136,-1.072 0.01,-0.13 0.014,-0.258 0.019,-0.388 0.005,-0.126 0.019,-0.25 0.019,-0.377 l 0,-288.48 z m -213.971,10.199 193.573,0 0,268.081 -193.573,0 z M 37.962,318.398 c -9.685,0 -17.564,-7.88 -17.564,-17.564 0,-9.684 7.879,-17.564 17.564,-17.564 l 239.668,0 0,7.306 -176.713,0 c -5.633,0 -10.199,4.567 -10.199,10.199 0,5.632 4.566,10.199 10.199,10.199 l 176.713,0 0,7.423 -239.668,0 z m 420.207,143.284 -160.14,0 0,-74.17 160.14,0 z m 24.263,-122.885 -19.435,28.318 -170.57,0 -25.023,-28.318 z" ] []
                    , Svg.path [ d "M338.012,210.149c0,5.632,4.567,10.199,10.199,10.199h33.029c5.632,0,10.199-4.567,10.199-10.199V131.82 c0-5.632-4.567-10.199-10.199-10.199h-33.029c-5.632,0-10.199,4.567-10.199,10.199V210.149z M358.41,142.019h12.631v18.766H358.41 V142.019z M371.041,199.95H358.41v-18.766h12.631V199.95z" ]
                        []
                    , Svg.path [ d "M324.129,255.69h81.194c5.632,0,10.199-4.567,10.199-10.199V76.417c0-5.632-4.567-10.199-10.199-10.199h-81.194 c-5.632,0-10.199,4.567-10.199,10.199v169.074C313.929,251.124,318.496,255.69,324.129,255.69z M334.328,86.616h60.795v148.676 h-60.795V86.616z" ] []
                    , Svg.path [ d "M69.675,290.576h-6.943c-5.633,0-10.199,4.567-10.199,10.199c0,5.632,4.566,10.199,10.199,10.199h6.943 c5.633,0,10.199-4.567,10.199-10.199C79.874,295.143,75.308,290.576,69.675,290.576z" ] []
                    ]
                ]
            ]


viewPackage : Package -> Svg.Svg msg
viewPackage package =
    Svg.circle
        [ cx (toString <| Vec2.getX package.position)
        , cy (toString <| Vec2.getY package.position)
        , r "3"
        , fill package.color
        , stroke package.color
        , strokeWidth "2"
        , strokeOpacity (toString package.glow)
        ]
        []


packPos : Vec2 -> Vec2 -> Float -> Vec2
packPos start dir time =
    let
        t =
            if time > 1.0 then
                time - 1.0
            else
                time
    in
        Vec2.add start <| Vec2.scale t dir


viewTemperatureSensor : TemperatureSensorData -> Svg.Svg msg
viewTemperatureSensor sensor =
    let
        pos =
            sensor.position

        temperature =
            sensor.temperature

        c =
            temperatureColor temperature

        h =
            99.0 * ((temperature - 20.0) / 50.0)
    in
        g []
            [ g [ transform (translation (Vec2.add pos <| vec2 20 -50)) ]
                [ g [ transform ("scale( 0.1)") ]
                    [ Svg.path [ d "m 366.62698,608.15939 c -9.64247,-1.62509 -18.33214,-8.15321 -22.82744,-17.14911 -6.31057,-12.62856 -3.22155,-28.25528 7.44058,-37.64047 2.63962,-2.32348 6.97834,-4.99844 9.6416,-5.94435 10.12734,-3.30195 10.1672,-2.98265 22.18471,-0.11274 22.22994,8.72935 27.56276,36.78101 10.03733,52.79826 -7.39597,6.75951 -16.92938,9.65747 -26.47678,8.04841 z", fill c ] []
                    , Svg.path [ d "m 355.32624,624.83474 c -11.80993,-4.02485 -21.47242,-12.17944 -28.02955,-23.65535 -3.94215,-6.89932 -5.74215,-14.42262 -5.74215,-24 0,-16.81121 6.29226,-29.86245 19.38596,-40.20984 l 5.11404,-4.0414 0.5,-45.12438 c 0.48605,-43.8657 0.56032,-45.23595 2.66237,-49.12438 4.81444,-8.90585 12.35439,-13.5 22.15629,-13.5 10.67981,0 17.88762,4.04468 24.76004,13.72522 1.5667,34.23928 1.11856,34.38418 1.21887,49.44055 l 0.29756,44.66576 5.31472,4.01266 c 11.76923,8.88585 19.53293,24.73358 19.57537,39.9584 0.062,22.22882 -15.90931,43.09733 -37.35361,48.80728 -8.21329,2.18695 -21.92851,1.74852 -29.85991,-0.95452 z m 26.94606,-11.16417 c 17.96613,-5.33537 30.56658,-24.68236 27.36277,-42.01335 -2.31706,-12.53415 -9.50334,-22.84108 -19.6059,-28.11977 l -5.46988,-2.85806 -0.006,-47 c -0.006,-44.84915 -0.0956,-47.14651 -1.95827,-50.20145 -5.57426,-9.1421 -18.32283,-7.78154 -22.66936,2.41933 -1.0949,2.56961 -1.37112,12.47225 -1.37112,49.15454 l 0,45.93666 -5.25097,2.96347 c -6.74331,3.80568 -13.22295,10.86374 -16.5729,18.0523 -3.67942,7.89557 -3.81968,21.01484 -0.31192,29.17515 7.7939,18.13144 27.68326,27.88717 45.85355,22.49118 z", fill "#000000" ] []
                    , rect [ height (toString h), fill c, width "14.596", x "364.131", y (toString (546.417 - h)) ] []
                    ]
                ]
              --  , (viewPort sensor.port_)
            ]


viewPort : PortData -> Svg.Svg msg
viewPort port_ =
    rect
        [ height "5.0"
        , width "5.0"
        , x (toString ((Vec2.getX port_.position) - 2.5))
        , y (toString ((Vec2.getY port_.position) - 2.5))
        , fill "#dddddd"
        , stroke "#999999"
        , strokeWidth "2"
        , strokeMiterlimit "6"
        , strokeLinejoin "round"
        ]
        []


viewSwitch : SwitchData -> Svg.Svg msg
viewSwitch switch =
    g []
        ([ rect
            [ height "10.0"
            , width "10.0"
            , x (toString ((Vec2.getX switch.position) - 5.0))
            , y (toString ((Vec2.getY switch.position) - 5.0))
            , fill "#999999"
            , stroke "#000000"
            , strokeWidth "2"
            , strokeMiterlimit "6"
            , strokeLinejoin "round"
            ]
            []
         ]
         --    ++ (List.map viewPort switch.ports)
        )


viewRouter : RouterData -> Svg.Svg msg
viewRouter router =
    g []
        ([ rect
            [ height "12.0"
            , width "32.0"
            , x (toString ((Vec2.getX router.position) - 16.0))
            , y (toString ((Vec2.getY router.position) - 6.0))
            , fill "#999999"
            , stroke "#000000"
            , strokeWidth "2"
            , strokeMiterlimit "6"
            , strokeLinejoin "round"
            ]
            []
         , rect
            [ height "10.0"
            , width (toString (Basics.min 30.0 (toFloat (List.length router.packageQueue))))
            , x (toString ((Vec2.getX router.position) - 15.0))
            , y (toString ((Vec2.getY router.position) - 5.0))
            , fill "#ff0000"
            ]
            []
         ]
        )



--
-- viewCableWithSomePackages : Vec2 -> Vec2 -> Float -> Svg.Svg msg
-- viewCableWithSomePackages start end time =
--     let
--         dir =
--             Vec2.sub end start
--     in
--         g
--             []
--             [ Svg.path [ d ("m " ++ (vec2xky start) ++ " " ++ (vec2xky dir)), stroke "#000000", strokeWidth "7.2" ] []
--             , viewPackage (packPos start dir (time + 0.0)) time "#ff0000"
--             , viewPackage (packPos start dir (time + 0.2)) time "#00ff00"
--             , viewPackage (packPos start dir (time + 0.4)) time "#0000ff"
--             , viewPackage (packPos start dir (time + 0.6)) time "#ffc600"
--             , viewPackage (packPos start dir (time + 0.8)) time "#ff0000"
--             , viewPackage (packPos start dir (time + 0.8875)) time "#00ff00"
--             , viewPackage (packPos start dir (time + 0.9)) time "#ff0000"
--             ]


viewCable : Simulation -> CableData -> Svg.Svg Msg
viewCable simulation cable =
    let
        port0 =
            getPort cable.port0 simulation

        port1 =
            getPort cable.port1 simulation

        dir =
            Vec2.sub port1.position port0.position
    in
        g []
            ([ Svg.path [ d ("m " ++ (vec2xky port0.position) ++ " " ++ (vec2xky dir)), stroke "#000000", strokeWidth "2.2" ] []
             ]
                ++ (List.map viewPackage cable.packages)
            )


viewFloatsVisualizer : FloatsVisualizerData -> Svg.Svg Msg
viewFloatsVisualizer v =
    let
        thetext =
            Dict.foldl (\id val t -> t ++ (toString id) ++ "=" ++ (toString val) ++ " | ") "" v.values
    in
        g []
            [ rect
                [ height "12.0"
                , width "32.0"
                , x (toString ((Vec2.getX v.position) - 16.0))
                , y (toString ((Vec2.getY v.position) - 6.0))
                , fill "#999999"
                , stroke "#000000"
                , strokeWidth "2"
                , strokeMiterlimit "6"
                , strokeLinejoin "round"
                ]
                []
            , text_
                [ x (toString ((Vec2.getX v.position) - 20.5))
                , y (toString ((Vec2.getY v.position) - 20.5))
                  --, text "hallo"
                ]
                [ Svg.text thetext ]
            ]


viewDevice : Simulation -> Simulation.Device -> Svg.Svg Msg
viewDevice simulation device =
    case device of
        Simulation.Machine machine ->
            viewMachine machine

        Simulation.TemperatureSensor sensor ->
            viewTemperatureSensor sensor

        Switch switch ->
            viewSwitch switch

        Router router ->
            viewRouter router

        Port port_ ->
            viewPort port_

        Cable cable ->
            viewCable simulation cable

        FloatsVisualizer v ->
            viewFloatsVisualizer v


viewSimulation : Simulation -> Html Msg
viewSimulation simulation =
    Html.div []
        [ svg
            [ version "1.1"
            , x "0"
            , y "0"
            , viewBox "0 0 1323.141 1322.95"
            ]
            (Simulation.mapDevices (\device -> viewDevice simulation device) simulation)
        ]
