port module Main exposing (..)

import NodeTest
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)


main : TestProgram
main =
    run emit
        NodeTest.all


port emit : ( String, Value ) -> Cmd msg
