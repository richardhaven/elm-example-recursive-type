module NodeTest exposing (..)

import Test exposing (..)
import SimpleTest exposing (simpleTest, negativeTest)
import RootTest exposing (rootTest)
import ThreeGenerationTest exposing (threeGenerationTest)


all : Test
all =
    describe "Node Test Suite" <|
        [ rootTest, simpleTest, negativeTest, threeGenerationTest ]
