module NodeTest exposing (..)

import Test exposing (..)
import Expect
import Debug exposing (log)


-- import Fuzz exposing (list, int, tuple, string)

import Node exposing (..)


all : Test
all =
    let
        firstRoot =
            Node.createRoot "rootKey" "root" "is a root"

        newRootNode =
            Debug.log "newRootNode"
                Node.createChild
                "childId"
                (itemOf firstRoot).id
                "childTitle"
                "child description"
                firstRoot

        x =
            log "firstRoot" (toString (Node.flatList firstRoot))
    in
        case newRootNode of
            Err message ->
                describe "Node Test Suite" <| [ test "Create childNode" <| \() -> Expect.fail message ]

            Ok rootNode ->
                describe "Node Test Suite" <|
                    [ describe "Test createRoot" <|
                        [ test "Create root id" <|
                            \() ->
                                Expect.equal (itemOf firstRoot).id "rootKey"
                        , test "Create root title" <|
                            \() ->
                                Expect.equal (itemOf firstRoot).title "root"
                        , test "Create root description" <|
                            \() ->
                                Expect.equal (itemOf firstRoot).description "is a root"
                        , test "Create root parentId" <|
                            \() ->
                                Expect.equal ((itemOf firstRoot).parentId) ""
                        , test "Create root children" <|
                            \() ->
                                Expect.equal (childrenOf firstRoot) []
                        , test "Check root-only flatlist" <|
                            \() -> Expect.equal (List.length (Node.flatList firstRoot)) 1
                        ]
                    ]
                        ++ case List.head (childrenOf rootNode) of
                            Nothing ->
                                [ describe "Node example Suite" <| [ test "Create childNode" <| \() -> Expect.fail "First child of the new root is Nothing" ] ]

                            Just firstChild ->
                                [ describe "Test createChild" <|
                                    [ test "log newRoot" <|
                                        \() ->
                                            Expect.notEqual (Just (log "rootNode" (toString rootNode))) Nothing
                                    , test "createChild id" <|
                                        \() ->
                                            Expect.equal (itemOf firstChild).id "childId"
                                    , test "createChild parentId" <|
                                        \() ->
                                            Expect.equal (itemOf firstChild).parentId "rootKey"
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
                                        \() -> Expect.equal (List.length (Node.flatList rootNode)) 2
                                    ]
                                ]
