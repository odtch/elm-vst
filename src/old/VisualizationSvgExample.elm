module VisualizationSvgExample exposing (..)

import VirtualDom as VD
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import String
import Time exposing (Time)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Simulation exposing (Simulation)
import VisualizationHtml as VH
import AnimationFrame
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Simulation exposing (Simulation)
import SimulationExample
import VisualizationSvg exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { simulation : Simulation
    , time : Time
    }


type Msg
    = NoOp
    | Animate Time
    | SimulationMsg Simulation.Msg
    | VisualizationSvgMsg VisualizationSvg.Msg


init : ( Model, Cmd Msg )
init =
    ( { simulation = SimulationExample.simulation1
      , time = 0
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Animate time ->
            let
                ( new_simulation, simulation_msg ) =
                    Simulation.simulate time model.simulation
            in
                { model | time = model.time + time, simulation = new_simulation } ! [ Cmd.map SimulationMsg simulation_msg ]

        SimulationMsg simulation_msg ->
            let
                ( newsim, new_simulation_cmd ) =
                    Simulation.update simulation_msg model.simulation
            in
                { model | simulation = newsim } ! [ Cmd.map SimulationMsg new_simulation_cmd ]

        VisualizationSvgMsg svgmsg ->
            let
                ( newsim, newsvgcmd ) =
                    VisualizationSvg.update svgmsg model.simulation
            in
                { model | simulation = newsim } ! [ Cmd.map VisualizationSvgMsg newsvgcmd ]


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.text "VisualizationSvgExample"
        , Html.map VisualizationSvgMsg (viewSimulation model.simulation)
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Animate
        ]
