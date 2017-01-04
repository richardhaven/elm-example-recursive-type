module RootTest exposing (..)

import Test exposing (..)
import Expect


-- import Fuzz exposing (list, int, tuple, string)

import Node exposing (..)


rootTest : Test
rootTest =
    let
        firstRoot =
            Node.createRoot "root Id" "root" "is a root"
    in
        describe "Test createRoot" <|
            [ test "Create root id" <|
                \() ->
                    Expect.equal (itemOf firstRoot).id "root Id"
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
                \() ->
                    Expect.equal (List.length (Node.flatList firstRoot)) 1
            , test "Check findNodesById for root" <|
                \() ->
                    Expect.notEqual (findNodeById "root Id" firstRoot) Nothing
            , test "Check findNodeByTitle for root" <|
                \() ->
                    Expect.equal (List.length (findNodesByTitle "root" firstRoot)) 1
            , test "Negative check findNodeById" <|
                \() ->
                    Expect.equal (findNodeById "XXXX" firstRoot) Nothing
            , test "Negative test findNodesByTitle" <|
                \() ->
                    Expect.equal (findNodesByTitle "XXXXXX" firstRoot) []
            ]
