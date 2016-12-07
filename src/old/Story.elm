module Story exposing (..)

import Time exposing (Time)


type alias Story msg =
    { events : List (Event msg)
    }



-- type alias Event msg =
--     { occursAt : Time
--     , msg : msg
--     }


type Event msg
    = Empty msg
    | Soon Time { occursAt : Time, msg : msg }


type alias Player msg =
    { story : Story msg
    , time : Time
    , paused : Bool
    , speed : Float
    }


empty : msg -> Story msg
empty msg =
    { events = [] }


andThen : msg -> Story msg -> Story msg
andThen message story =
    andIn 0 message story


andInS : Float -> Story msg -> Story msg
andInS secs message story =
    andIn (Time.second * secs) message story


andIn : Time -> Story msg -> Story msg
andIn time message story =
    let
        event =
            Soon time { occursAt = (lastEventOccursAt story), msg = message }
    in
        { story
            | events = story.events ++ [ event ]
        }


andAt : Time -> Story msg -> Story msg
andAt time message story =
    let
        newEvent =
            Soon time { occursAt = 0, msg = message }
    in
        { story | events = newEvent :: story.events }


lastEventOccursAt : Story msg -> Time
lastEventOccursAt story =
    List.foldl maxEventTime 0 story.events


maxEventTime event acc =
    case event of
        Empty message ->
            0

        Soon time data ->
            max time data.occursAt


start : Story msg -> Player msg
start story =
    { story = story
    , time = 0
    , paused = False
    , speed = 1
    }


setPaused : Bool -> Player msg -> Player msg
setPaused paused player =
    { player | paused = paused }


setSpeed : Float -> Player msg -> Player msg
setSpeed speed player =
    if (speed <= 0 || 100 < speed) then
        Debug.crash "speed out of range"
    else
        { player | speed = speed }


tick : Time -> (msg -> model -> ( model, Cmd msg )) -> Player msg -> model -> ( Player msg, model, Cmd msg )
tick time update player model =
    if (player.paused) then
        ( player, model, Cmd.none )
    else
        let
            newtime =
                player.time + time * player.speed

            events =
                List.filter (occursIn player.time newtime) player.story.events

            ( newmodel, msg ) =
                executeEvents update events model
        in
            ( { player
                | time = newtime
              }
            , newmodel
            , msg
            )


executeEvents : (msg -> model -> ( model, Cmd msg )) -> List (Event msg) -> model -> ( model, Cmd msg )
executeEvents update events model =
    List.foldl
        (\event ( premodel, precmd ) ->
            case event of
                Empty message ->
                    ( premodel, precmd )

                Soon time data ->
                    update data.msg premodel
        )
        ( model, Cmd.none )
        events


occursIn : Time -> Time -> Event msg -> Bool
occursIn start end event =
    case event of
        Empty message ->
            False

        Soon time data ->
            (start <= data.occursAt) && (data.occursAt < end)
