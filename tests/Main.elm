port module Main exposing (..)

import Test exposing (..)
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)
import StoryTest


main : TestProgram
main =
    run emit <|
        describe "VST - Story Test Suite"
            [ --Tests.all,
              StoryTest.all
            ]


port emit : ( String, Value ) -> Cmd msg
