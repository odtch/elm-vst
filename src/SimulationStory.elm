module SimulationStory exposing (..)

import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import String
import Task
import Time exposing (..)
import SimulationTypes exposing (..)
import Simulation exposing (..)
import SimulationUpdate
import SimulationTick
import Machine
import Animation
import Story exposing (..)
import AnimationFrame
import Example
import VisualizationHtml exposing (..)


type alias Model =
    { simulation : Simulation
    , player : Story.Player Msg
    }


create : Story Msg -> Model
create story =
    { simulation = Simulation.empty
    , player = Story.start story
    }


init : ( Model, Cmd Msg )
init =
    ( { simulation = Example.sim3
      , player = Story.start Story.empty
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( new_simulation, simulation_msgs ) =
            SimulationUpdate.update msg model.simulation
    in
        ( { model
            | simulation =
                new_simulation
          }
          --, (msgToCmd (SimulationMessage simulation_msgs))
        , simulation_msgs
        )


tick : Time -> Model -> ( Model, Cmd Msg )
tick time model =
    let
        ( new_simulation, simulation_msgs ) =
            SimulationTick.tick time model.simulation

        ( new_player, story_msgs ) =
            Story.tick time model.player

        result_msgs =
            Cmd.batch
                [ simulation_msgs
                , Cmd.batch ((List.map (\msg -> msgToCmd msg) story_msgs))
                ]
    in
        ( { model
            | simulation = new_simulation
            , player = new_player
          }
          --, (msgToCmd (SimulationMessage simulation_msgs))
          --, (Cmd.batch ([ simulation_msgs, (msgToCmd story_msgs) ]))
        , result_msgs
        )
