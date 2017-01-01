module SimpleTest exposing (..)

import Test exposing (..)
import Expect
import Node exposing (..)


simpleTest : Test
simpleTest =
    let
        firstRoot =
            Node.createRoot "rootId" "root" "is a root"

        secondRoot =
            Node.createChild
                "childId"
                "rootId"
                "childTitle"
                "child description"
                firstRoot
    in
        case secondRoot of
            Err message ->
                describe "Test createChild" <| [ test "Create childNode" <| \() -> Expect.fail message ]

            Ok rootNode ->
                case List.head (childrenOf rootNode) of
                    Nothing ->
                        describe "Test createChild" <|
                            [ test "Create childNode" <|
                                \() -> Expect.fail "First child of the new root is Nothing"
                            ]

                    Just firstChild ->
                        describe "Test createChild" <|
                            [ test "createChild id" <|
                                \() ->
                                    Expect.equal (itemOf firstChild).id "childId"
                            , test "createChild parentId" <|
                                \() ->
                                    Expect.equal (itemOf firstChild).parentId "rootId"
                            , test "createChild root child count" <|
                                \() ->
                                    Expect.equal (List.length (childrenOf rootNode)) 1
                            , test "createChild root children" <|
                                \() ->
                                    Expect.equal (itemOf firstChild).id "childId"
                            , test "createChild new child count" <|
                                \() ->
                                    Expect.equal (List.length (childrenOf firstChild)) 0
                            , test "Check one-child flatlist" <|
                                \() ->
                                    Expect.equal (List.length (Node.flatList rootNode)) 2
                            , test "Check findNodeById for root" <|
                                \() ->
                                    Expect.notEqual (findNodeById "rootId" rootNode) Nothing
                            , test "Check findNodesByTitle for root" <|
                                \() ->
                                    Expect.notEqual (findNodesByTitle "root" rootNode) []
                            , test "Check findNodeById" <|
                                \() ->
                                    Expect.notEqual (findNodeById "childId" rootNode) Nothing
                            , test "Check findNodesByTitle" <|
                                \() ->
                                    Expect.notEqual (findNodesByTitle "childTitle" rootNode) []
                            , test "Negative check findNodeById" <|
                                \() ->
                                    Expect.equal (findNodeById "XXXX" rootNode) Nothing
                            , test "Negative test findNodeByTitle" <|
                                \() ->
                                    Expect.equal (findNodesByTitle "XXXXXX" rootNode) []
                            ]


duplicateChildIdTest : Test
duplicateChildIdTest =
    let
        firstRoot =
            Node.createRoot "rootId" "root" "is a root"

        secondRoot =
            Node.createChild
                "same id"
                "rootId"
                "childTitle"
                "child description"
                firstRoot

        expectedFailure =
            Node.createChild
                "same id"
                "rootId"
                "childTitle"
                "child description"
                firstRoot
    in
        case expectedFailure of
            Err message ->
                describe "Test createChild duplicate id" <|
                    [ test "createChild duplicate id" <|
                        \() -> Expect.true "duplicate id rejected" True
                    ]

            Ok _ ->
                describe "Test createChild duplicate id" <|
                    [ test "createChild duplicate id" <|
                        \() -> Expect.fail "duplicate id not rejected"
                    ]
