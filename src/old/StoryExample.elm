module StoryExample exposing (..)

import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import String
import Time exposing (Time)
import Story exposing (Story)
import SubTitles
import Task
import Animation
import AnimationFrame


story1 : Story Msg


story1x =
    Story.andThen (ShowMessage "Die Animationen werden dadurch nicht schneller.") <|
        Story.andInS 1 (ShowMessage "Die Geschwindigkeit beeinflusst nur die Story.") <|
            Story.andInS 2 (ShowMessage "Sie können die Story pausieren und wieder starten.") <|
                Story.andInS 2 (ShowMessage "Unten können Sie neue Stories starten.") <|
                    Story.andThen (ShowMessage "Mit dem Player werden die Events abgespielt.") <|
                        Story.andInS 1 (ShowMessage "Die Story besteht aus Events.") <|
                            Story.andInS 1 (ShowMessage "Story") <|
                                Story.andThen (ShowMessage "Dieses Beispiel demonstriert die Fähigkeiten von") <|
                                    Story.empty NoOp


story1 =
    [ Wait 5 * Time.second
    , ShowMessage "Hallo"
    , ShowMessage "Welt"
    , Wait 1 * Time.second
    , ShowMessage "Welt"
    ]



-- story2 : Story Msg
-- story2 =
--     { events =
--         [ { occursIn = Time.second * 0, msg = (ShowMessage "Story 2") }
--         ]
--     }


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { story : Story Msg
    , storySpeed : Float
    , paused : Bool
    , player : Story.Player Msg
    , subtitles : SubTitles.Model
    }


type Msg
    = NoOp
    | Animate Time
    | ShowMessage String
    | StartStory (Story Msg)
    | SetStorySpeedFromString String
    | SetStorySpeed Float
    | Pause
    | Continue


init : ( Model, Cmd Msg )
init =
    ( { story = story1
      , storySpeed = 1.0
      , paused = False
      , player = Story.start story1
      , subtitles = SubTitles.default
      }
    , Cmd.none
    )


startStory : Story Msg -> Model -> ( Model, Cmd Msg )
startStory newstory model =
    ( { model
        | story = newstory
        , player = Story.setPaused model.paused <| Story.setSpeed model.storySpeed <| Story.start newstory
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Animate time ->
            animate time model

        ShowMessage message ->
            { model | subtitles = SubTitles.addMessage message model.subtitles } ! []

        StartStory story ->
            startStory story model

        SetStorySpeedFromString speedString ->
            update (SetStorySpeed (0.05 * (Result.withDefault 0 (String.toFloat speedString)))) model

        SetStorySpeed speed ->
            { model | storySpeed = speed, player = Story.setSpeed speed model.player } ! []

        Pause ->
            { model | paused = True, player = Story.setPaused True model.player } ! []

        Continue ->
            { model | paused = False, player = Story.setPaused False model.player } ! []


animate : Time -> Model -> ( Model, Cmd Msg )
animate time model =
    let
        ( newplayer, newmodel, msg ) =
            Story.tick time update model.player model
    in
        { newmodel
            | player = newplayer
            , subtitles = SubTitles.animate time newmodel.subtitles
        }
            ! [ msg ]


view : Model -> Html Msg
view model =
    Html.div []
        [ SubTitles.view model.subtitles
        , Html.div []
            [ Html.button [ HE.onClick (StartStory story1) ] [ Html.text "Story 1" ]
              --, Html.button [ HE.onClick (StartStory story2) ] [ Html.text "Story 2" ]
            ]
        , Html.div []
            [ Html.button [ HE.onClick Pause ] [ Html.text "Pause" ]
            , Html.button [ HE.onClick Continue ] [ Html.text "Continue" ]
            ]
        , Html.div []
            [ Html.text "Story-Geschwindigkeit:"
            , Html.input
                [ HA.type_ "range"
                , HA.value (toString (model.player.speed / 0.05))
                , HE.onInput SetStorySpeedFromString
                ]
                []
            , Html.text <| toString model.player.speed
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Animate
        ]
