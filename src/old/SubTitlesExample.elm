module SubTitlesExample exposing (..)

import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import String
import Task
import Time exposing (Time)
import SubTitles
import Animation
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
    { subtitles : SubTitles.Model
    }


type Msg
    = NoOp
    | Animate Time
    | ShowMessage String


init : ( Model, Cmd Msg )
init =
    ( { subtitles =
            SubTitles.addMessageWithTimeToDestroy (Time.second * 6) "SubTitles" <|
                SubTitles.addMessage "Dieses Beispiel zeigt die Möglichkeiten von" <|
                    SubTitles.default
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Animate time ->
            { model
                | subtitles = SubTitles.animate time model.subtitles
            }
                ! []

        ShowMessage message ->
            { model | subtitles = SubTitles.addMessage message model.subtitles } ! []


view : Model -> Html Msg
view model =
    Html.div []
        [ SubTitles.view model.subtitles
        , Html.div []
            [ Html.button [ HE.onClick (ShowMessage "Neuer SubTitle") ] [ Html.text "Sub-Titel erstellen" ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Animate
        ]
