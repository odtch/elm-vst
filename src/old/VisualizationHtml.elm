module VisualizationHtml exposing (..)

import VirtualDom as VD
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import String
import Time exposing (Time)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Simulation exposing (..)


viewDevice : Device -> Html.Html msg
viewDevice device =
    Html.div
        []
        [ Html.text
            (case device of
                Machine machine ->
                    "machine " ++ (toString machine.id) ++ " with speed " ++ (toString machine.speed) ++ " produced " ++ (toString machine.produced)

                TemperatureSensor sensor ->
                    "temperatur sensor "
                        ++ (toString sensor.id)
                        ++ " for "
                        ++ (toString sensor.machineId)
                        ++ " with value "
                        ++ (toString sensor.temperature)
                        ++ " "
                        ++ sensor.error

                Switch switch ->
                    "switch " ++ (toString switch.id)

                _ ->
                    "a device with unknown type"
            )
        ]


view : Simulation -> Html.Html msg
view simulation =
    Html.div []
        (Simulation.mapDevices viewDevice simulation)
