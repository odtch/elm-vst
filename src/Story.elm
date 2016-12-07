module Story exposing (..)

import Time exposing (Time)


type Action msg
    = Wait Time
    | Send msg


type alias Event msg =
    { occursAt : Time
    , msg : msg
    }


type alias Story msg =
    { events : List (Event msg) }


type alias Player msg =
    { story : Story msg
    , time : Time
    , paused : Bool
    , speed : Float
    }


empty : Story msg
empty =
    { events = [] }


create : List (Action msg) -> Story msg
create actions =
    let
        ( time, events ) =
            List.foldl
                (\action ( time, events ) ->
                    case action of
                        Wait delay ->
                            ( time + delay, events )

                        Send msg ->
                            ( time, events ++ [ { occursAt = time, msg = msg } ] )
                )
                ( 0, [] )
                actions
    in
        { events = events
        }


messagesBetween : Time -> Time -> Story msg -> List msg
messagesBetween start end story =
    List.map (\event -> event.msg) <| List.filter (\event -> occursIn start end event) story.events


occursIn : Time -> Time -> Event msg -> Bool
occursIn start end event =
    (start <= event.occursAt) && (event.occursAt < end)


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


tick : Time -> Player msg -> ( Player msg, List msg )
tick time player =
    if (player.paused) then
        ( player, [] )
    else
        let
            newtime =
                player.time + time * player.speed
        in
            ( { player
                | time = newtime
              }
            , messagesBetween player.time newtime player.story
            )



--
--
-- tick : Time -> (msg -> model -> ( model, Cmd msg )) -> Player msg -> model -> ( Player msg, model, Cmd msg )
-- tick time update player model =
--     if (player.paused) then
--         ( player, model, Cmd.none )
--     else
--         let
--             newtime =
--                 player.time + time * player.speed
--
--             events =
--                 List.filter (occursIn player.time newtime) player.story.events
--
--             ( newmodel, msg ) =
--                 executeEvents update events model
--         in
--             ( { player
--                 | time = newtime
--               }
--             , newmodel
--             , msg
--             )
--
--
-- executeEvents : (msg -> model -> ( model, Cmd msg )) -> List (Event msg) -> model -> ( model, Cmd msg )
-- executeEvents update events model =
--     List.foldl
--         (\event ( premodel, precmd ) ->
--             case event of
--                 Empty message ->
--                     ( premodel, precmd )
--
--                 Soon time data ->
--                     update data.msg premodel
--         )
--         ( model, Cmd.none )
--         events
--
--
