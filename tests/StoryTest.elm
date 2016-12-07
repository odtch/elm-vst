module StoryTest exposing (..)

import Test exposing (..)
import Time exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Story exposing (Story)


story : Story String
story =
    Story.create
        [ Story.Send "A"
        , Story.Wait <| 1 * second
        , Story.Send "B"
        , Story.Send "C"
        , Story.Wait <| 2 * second
        , Story.Send "D"
        ]


eventsToString : List (Story.Event String) -> String
eventsToString events =
    List.foldl
        (\event text ->
            (if (String.length text == 0) then
                ""
             else
                text ++ ", "
            )
                ++ "at "
                ++ (toString <| inSeconds event.occursAt)
                ++ " "
                ++ event.msg
        )
        ""
        events


messagesToString : List String -> String
messagesToString messages =
    List.foldl
        (\message text ->
            (if (String.length text == 0) then
                ""
             else
                text ++ ", "
            )
                ++ message
        )
        ""
        messages


runPlayer : Time -> Story String -> String
runPlayer stepTime story =
    let
        stepCount =
            round <| (second * 6.0) / stepTime

        ticks =
            List.repeat stepCount stepTime

        player =
            Story.start story

        ( newplayer, text ) =
            (List.foldr
                (\time ( player, text ) ->
                    let
                        ( newplayer, messages ) =
                            Story.tick time player
                    in
                        ( newplayer, text ++ (messagesToString messages) ++ " | " )
                )
                ( player, "" )
                ticks
            )
    in
        text


all : Test
all =
    describe "Story Test"
        [ test "all events" <|
            \() -> Expect.equal (eventsToString story.events) "at 0 A, at 1 B, at 1 C, at 3 D"
        , test "events 0 - 1" <|
            \() -> Expect.equal (messagesToString (Story.messagesBetween (0 * second) (1 * second) story)) "A"
        , test "events 0 - 2" <|
            \() -> Expect.equal (messagesToString (Story.messagesBetween (0 * second) (2 * second) story)) "A, B, C"
        , test "events 0 - 4" <|
            \() -> Expect.equal (messagesToString (Story.messagesBetween (0 * second) (4 * second) story)) "A, B, C, D"
        , test "events 1 - 2" <|
            \() -> Expect.equal (messagesToString (Story.messagesBetween (1 * second) (2 * second) story)) "B, C"
        , test "events 1 - 4" <|
            \() -> Expect.equal (messagesToString (Story.messagesBetween (1 * second) (4 * second) story)) "B, C, D"
        , test "events 3 - 4" <|
            \() -> Expect.equal (messagesToString (Story.messagesBetween (3 * second) (4 * second) story)) "D"
        , test "events 4 - 5" <|
            \() -> Expect.equal (messagesToString (Story.messagesBetween (4 * second) (5 * second) story)) ""
        , test "player 1s" <|
            \() -> Expect.equal (runPlayer (1 * second) story) "A | B, C |  | D |  |  | "
        , test "player 0.5s" <|
            \() -> Expect.equal (runPlayer (0.5 * second) story) "A |  | B, C |  |  |  | D |  |  |  |  |  | "
        , test "player 2s" <|
            \() -> Expect.equal (runPlayer (2 * second) story) "A, B, C | D |  | "
        ]
