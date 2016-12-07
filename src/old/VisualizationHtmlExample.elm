module VisualizationHtmlExample exposing (..)

import VirtualDom as VD
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import String
import Time exposing (Time)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Simulation exposing (Simulation)
import SimulationExample
import VisualizationHtml as VH
import AnimationFrame


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
    }


type Msg
    = NoOp
    | Animate Time


init : ( Model, Cmd Msg )
init =
    ( { simulation = SimulationExample.simulation1
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Animate time ->
            { model | simulation = Simulation.simulate time model.simulation } ! []


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.text "SimulationExample"
        , VH.view model.simulation
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Animate
        ]
