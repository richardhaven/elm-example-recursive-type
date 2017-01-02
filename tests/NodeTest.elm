module NodeTest exposing (..)

import Test exposing (..)
import SimpleTest exposing (simpleTest, duplicateChildIdTest)
import RootTest exposing (rootTest)
import ThreeGenerationTest exposing (threeGenerationTest)


all : Test
all =
    describe "Node Test Suite" <|
        [ rootTest, simpleTest, duplicateChildIdTest, threeGenerationTest ]
