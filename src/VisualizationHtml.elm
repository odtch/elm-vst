module VisualizationHtml exposing (..)

import VirtualDom as VD
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import String
import Time exposing (Time)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Simulation exposing (..)
import SimulationTypes exposing (..)


viewDevice : Device -> Html.Html msg
viewDevice device =
    Html.div
        []
        [ Html.text
            (case device.type_ of
                MachineType machine ->
                    "machine "
                        ++ (toString device.id)
                        ++ " speed="
                        ++ (toString machine.speed)
                        ++ " produced="
                        ++ (toString machine.produced)
                        ++ " temperature="
                        ++ (toString machine.temperature)

                SensorType sensor ->
                    "sensor "
                        ++ (toString device.id)
                        ++ " lastValue="
                        ++ (toString sensor.lastValue)

                _ ->
                    "a device with unknown type"
            )
        ]


view : Simulation -> Html.Html msg
view simulation =
    Html.div []
        [ Html.div [] <| List.map (\error -> Html.div [] [ Html.text ("Error: " ++ error) ]) simulation.errors
        , Html.div []
            (mapDevices viewDevice simulation)
        ]
