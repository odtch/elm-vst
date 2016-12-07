module SubTitles exposing (..)

import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import String
import Time exposing (Time)


type alias Model =
    { fontsize : Int
    , defaultTimeToDestroy : Time
    , messages : List Message
    }


type alias Message =
    { timeToDestroy : Time
    , title : String
    }


default : Model
default =
    init 22 (2 * Time.second)


init : Int -> Time -> Model
init fontsize defaultTimeToDestroy =
    { fontsize = fontsize
    , defaultTimeToDestroy = defaultTimeToDestroy
    , messages = []
    }


animate : Time -> Model -> Model
animate time model =
    { model
        | messages = List.filter isMessageAlive <| List.map (animateMessage time) model.messages
    }


animateMessage : Time -> Message -> Message
animateMessage time message =
    { message
        | timeToDestroy = message.timeToDestroy - time
    }


isMessageAlive : Message -> Bool
isMessageAlive message =
    message.timeToDestroy > 0


view : Model -> Html msg
view model =
    Html.div
        [ HA.style
            [ ( "min-height", "100px" )
            , ( "max-height", "100px" )
            , ( "border", "1px solid #000000" )
            , ( "margin", "2px" )
            ]
        ]
        [ Html.div [] (List.map (viewMessage model) model.messages)
        ]


viewMessage : Model -> Message -> Html msg
viewMessage model message =
    let
        fontsize =
            (toFloat model.fontsize) * (min 1.0 (message.timeToDestroy / 1000))
    in
        Html.div
            [ HA.style [ ( "font-size", (toString fontsize) ++ "px" ) ]
            ]
            [ Html.text <| message.title
            ]


addMessage : String -> Model -> Model
addMessage title model =
    addMessageWithTimeToDestroy model.defaultTimeToDestroy title model


addMessageWithTimeToDestroy : Float -> String -> Model -> Model
addMessageWithTimeToDestroy timeToDestroy title model =
    { model
        | messages =
            model.messages
                ++ [ { timeToDestroy = timeToDestroy
                     , title = title
                     }
                   ]
    }
