module Main exposing (..)

import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import String
import Task
import Time exposing (Time)
import SimulationTypes exposing (..)
import Simulation exposing (..)
import SimulationUpdate
import SimulationTick
import SimulationStory exposing (..)
import SimulationStoryExample exposing (..)
import Machine
import Animation
import Story
import AnimationFrame
import VisualizationHtml exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { simstory : SimulationStory.Model
    }


type Msg
    = NoOp
    | Animate Time
    | SimulationMessage SimulationTypes.Msg


init : ( Model, Cmd Msg )
init =
    ( { simstory = SimulationStory.create SimulationStoryExample.story1
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        SimulationMessage simulation_msg ->
            let
                ( new_simulation, simulation_msgs ) =
                    SimulationStory.update simulation_msg model.simstory
            in
                ( { model
                    | simstory =
                        new_simulation
                  }
                  --, (msgToCmd (SimulationMessage simulation_msgs))
                , Cmd.map (\a -> SimulationMessage a) simulation_msgs
                )

        Animate time ->
            let
                ( new_simulation, simulation_msgs ) =
                    SimulationStory.tick time model.simstory
            in
                ( { model
                    | simstory =
                        new_simulation
                  }
                  --, (msgToCmd (SimulationMessage simulation_msgs))
                , Cmd.map (\a -> SimulationMessage a) simulation_msgs
                )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div []
            [--Html.button [ HE.onClick (ShowMessage "Neuer SubTitle") ] [ Html.text "Sub-Titel erstellen" ]
            ]
          --, Html.div [] <| List.map (\error -> Html.div [] [ Html.text error ]) model.simulation.errors
          --, Html.div [] [ Html.text <| Simulation.toString model.simulation ]
        , VisualizationHtml.view model.simstory.simulation
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Animate
        ]
