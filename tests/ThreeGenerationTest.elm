module ThreeGenerationTest exposing (..)

import Expect
import Test exposing (..)


-- import Fuzz exposing (list, int, tuple, string)

import Node exposing (..)


threeGenerationTest : Test
threeGenerationTest =
    let
        firstRoot =
            Node.createRoot "rootKey" "root" "is a root"

        lastRoot =
            Node.createChildren
                [ { id = "child1 Id"
                  , parentId = "root"
                  , title = "child1 title"
                  , description = "child1 description"
                  }
                , { id = "child2 Id"
                  , parentId = "root"
                  , title = "child2 title"
                  , description = "child2 description"
                  }
                , { id = "child of child2 Id"
                  , parentId = "child2 Id"
                  , title = "grandchild title"
                  , description = "grandchild description"
                  }
                ]
                firstRoot
    in
        case lastRoot of
            Err message ->
                describe "Test createGrandchildren" <| [ test "Create children and grandchildren Nodes" <| \() -> Expect.fail message ]

            Ok rootNode ->
                describe "Test createChildren" <|
                    [ test "createChild id" <|
                        \() ->
                            Expect.notEqual (findNodeById "child1 Id" rootNode) Nothing
                    , test "Check flatlist" <|
                        \() ->
                            Expect.equal (List.length (Node.flatList rootNode)) 4
                    , test "Check findNodeById for root" <|
                        \() ->
                            Expect.notEqual (findNodeById "rootId" rootNode) Nothing
                    , test "Check findNodesByTitle for root" <|
                        \() ->
                            Expect.notEqual (findNodesByTitle "root" rootNode) []
                    , test "Check findNodeById" <|
                        \() ->
                            Expect.notEqual (findNodeById "child1 Id" rootNode) Nothing
                    , test "Check findNodesByTitle" <|
                        \() ->
                            Expect.notEqual (findNodesByTitle "child1 Title" rootNode) []
                    , test "Check findNodeById" <|
                        \() ->
                            Expect.notEqual (findNodeById "child2 Id" rootNode) Nothing
                    , test "Check findNodesByTitle" <|
                        \() ->
                            Expect.notEqual (findNodesByTitle "child2 Title" rootNode) []
                    , test "Check findNodeById" <|
                        \() ->
                            Expect.notEqual (findNodeById "child of child2 Id" rootNode) Nothing
                    , test "Check findNodesByTitle" <|
                        \() ->
                            Expect.notEqual (findNodesByTitle "grandchild title" rootNode) []
                    , test "Negative check findNodeById" <|
                        \() ->
                            Expect.equal (findNodeById "XXXX" rootNode) Nothing
                    , test "Negative test findNodeByTitle" <|
                        \() ->
                            Expect.equal (findNodesByTitle "XXXXXX" rootNode) []
                    ]


isChildAttributeEqual : Maybe Node Item -> String -> String -> Bool
isChildAttributeEqual maybeNode attributeName value =
    case maybeNode of
        Nothing ->
            False

        Just node ->
            case attributeName of
                "id" ->
                    (itemOf node).id == value

                "title" ->
                    (itemOf node).title == value

                "parentId" ->
                    (itemOf node).parentId == value

                _ ->
                    False
