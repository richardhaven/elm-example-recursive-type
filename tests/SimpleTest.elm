module SimpleTest exposing (..)

import Test exposing (..)
import Expect
import Node exposing (..)


--import Debug exposing (log)


simpleTest : Test
simpleTest =
    let
        firstRoot =
            Node.createRoot "simple root id" "root title" "is a root"

        secondRoot =
            Node.createChild
                "child id"
                "simple root id"
                "child title"
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
                        describe "Simple tests" <|
                            [ describe "createChild test" <|
                                [ test "createChild id" <|
                                    \() ->
                                        Expect.equal (itemOf firstChild).id "child id"
                                , test "createChild parentId" <|
                                    \() ->
                                        Expect.equal (itemOf firstChild).parentId "simple root id"
                                , test "createChild root child count" <|
                                    \() ->
                                        Expect.equal (List.length (childrenOf rootNode)) 1
                                , test "createChild root children" <|
                                    \() ->
                                        Expect.equal (itemOf firstChild).id "child id"
                                , test "createChild new child count" <|
                                    \() ->
                                        Expect.equal (List.length (childrenOf firstChild)) 0
                                , test "Check one-child flatlist" <|
                                    \() ->
                                        Expect.equal (List.length (Node.flatList rootNode)) 2
                                ]
                            , describe "function tests"
                                [ test "Check findNodeById for root" <|
                                    \() ->
                                        Expect.notEqual (findNodeById "simple root id" rootNode) Nothing
                                , test "Check findNodesByTitle for root" <|
                                    \() ->
                                        Expect.equal (List.length (findNodesByTitle "root title" rootNode)) 1
                                , test "Check findNodeById" <|
                                    \() ->
                                        Expect.notEqual (findNodeById "child id" rootNode) Nothing
                                , test "Check findNodesByTitle" <|
                                    \() ->
                                        Expect.equal (List.length (findNodesByTitle "child title" rootNode)) 1
                                , test "Check findNodesByTitle case insensitive" <|
                                    \() ->
                                        Expect.equal (List.length (findNodesByTitle "Child TITLE" rootNode)) 1
                                , test "Check findNodesByTitle ending substring" <|
                                    \() ->
                                        Expect.equal (List.length (findNodesByTitle "d title" rootNode)) 1
                                , test "Check findNodesByTitle starting substring" <|
                                    \() ->
                                        Expect.equal (List.length (findNodesByTitle "child titl" rootNode)) 1
                                , test "Check findNodesByTitle substring" <|
                                    \() ->
                                        Expect.equal (List.length (findNodesByTitle "title" rootNode)) 2
                                ]
                            , describe "negative function tests"
                                [ test "Negative check findNodeById" <|
                                    \() ->
                                        Expect.equal (findNodeById "XXXX" rootNode) Nothing
                                , test "Negative test findNodeByTitle" <|
                                    \() ->
                                        Expect.equal (findNodesByTitle "XXXXXX" rootNode) []
                                ]
                            ]


negativeTest : Test
negativeTest =
    let
        firstRoot =
            Node.createRoot "simple root id" "root" "is a root"

        secondRootMaybe =
            Node.createChild
                "same id"
                "simple root id"
                "childTitle"
                "child description"
                firstRoot
    in
        describe "Test createChild duplicate id" <|
            case secondRootMaybe of
                Err message ->
                    [ test "createChild create first child" <|
                        \() -> Expect.fail message
                    ]

                Ok secondRoot ->
                    case Node.createChild "same id" "simple root id" "childTitle" "child description" secondRoot of
                        Err _ ->
                            [ test "createChild create duplicate child" <|
                                \() -> Expect.true "Duplicate id rejected" True
                            ]

                        Ok _ ->
                            [ test "createChild create duplicate child" <|
                                \() -> Expect.fail ("duplicate id not rejected: " ++ toString secondRoot)
                            ]
