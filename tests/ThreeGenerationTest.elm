module ThreeGenerationTest exposing (..)

import Expect
import Test exposing (..)


-- import Fuzz exposing (list, int, tuple, string)

import Node exposing (..)


threeGenerationTest : Test
threeGenerationTest =
    describe "Test three and four generations" <|
        [ threeGenerationCreationTest
        , threeGenerationNavigationTest
        , threeGenerationUpdateItemTest
        , threeGenerationNewParentChangeParentTest
        , threeGenerationOldParentChangeParentTest
        , threeGenerationParentIdChangeParentTest
        , threeGenerationChangeParentComplexTest
        , threeGenerationDeleteLeafItemTest
        , threeGenerationDeleteBranchItemTest
        , threeGenerationOldParentDeleteLeafItemTest
        , threeGenerationDeleteItemCheckNewParentTest
        , threeGenerationDeleteItemCheckOldParentTest
        , threeGenerationNegativeTest
        , createDuplicateChildren
        ]


simpleChildren : List Item
simpleChildren =
    [ { id = "child1 id"
      , parentId = "complex root id"
      , title = "child1 title"
      , description = "child1 description"
      }
    , { id = "child2 id"
      , parentId = "complex root id"
      , title = "child2 title"
      , description = "child2 description"
      }
    , { id = "grandchild id"
      , parentId = "child2 id"
      , title = "grandchild title"
      , description = "grandchild description"
      }
    ]


createThreeGenerations : Result String (Node Item)
createThreeGenerations =
    let
        firstRoot =
            Node.createRoot "complex root id" "root title" "is a root"
    in
        Node.createChildren simpleChildren firstRoot


moreChildren : List Item
moreChildren =
    [ { id = "child1 child1 id", parentId = "child1 id", title = "child11 title", description = "child11 description" }
    , { id = "child1 child2 id", parentId = "child1 id", title = "child12 title", description = "child12 description" }
    , { id = "child2 child2 id", parentId = "child2 id", title = "child22 title", description = "child22 description" }
    , { id = "grandchild child id", parentId = "grandchild id", title = "child211 title", description = "child211 description" }
    ]


{-|
                                                root
                                  ------------- | -----------
                                  |                         |
                                child1                    child2
                                  | - child1 child1 id      | - child2 child2 id
                                  | - child1 child2 id      | - grandchild id
                                                                     |
                                                              grandchild child id



-}
createMoreThreeGenerations : Result String (Node Item)
createMoreThreeGenerations =
    let
        firstRoot =
            Node.createRoot "complex root id" "root title" "is a root"
    in
        Node.createChildren (List.append simpleChildren moreChildren) firstRoot


threeGenerationCreationTest : Test
threeGenerationCreationTest =
    let
        lastRoot =
            createThreeGenerations
    in
        case lastRoot of
            Err message ->
                describe "Test createGrandchildren" <| [ test "Create children and grandchildren Nodes" <| \() -> Expect.fail message ]

            Ok rootNode ->
                describe "Test createGrandchildren" <|
                    [ test "createChild id" <|
                        \() ->
                            Expect.notEqual (findNodeById "child1 id" rootNode) Nothing
                    , test "Check isItemMember" <|
                        \() ->
                            Expect.true "isItemMember \"child1 id\"" (isItemMember "child1 id" rootNode)
                    , test "Check flatlist" <|
                        \() ->
                            Expect.equal (List.length (Node.flatList rootNode)) 4
                    , test "Check findNodeById for root" <|
                        \() ->
                            Expect.notEqual (findNodeById "complex root id" rootNode) Nothing
                    , test "Check findNodesByTitle for root" <|
                        \() ->
                            Expect.notEqual (findNodesByTitle "root title" rootNode) []
                    , test "Check findNodeById" <|
                        \() ->
                            Expect.notEqual (findNodeById "child1 id" rootNode) Nothing
                    , test "Check findNodesByTitle" <|
                        \() ->
                            Expect.notEqual (findNodesByTitle "child1 Title" rootNode) []
                    , test "Check findNodeById" <|
                        \() ->
                            Expect.notEqual (findNodeById "child2 id" rootNode) Nothing
                    , test "Check findNodesByTitle" <|
                        \() ->
                            Expect.notEqual (findNodesByTitle "child2 Title" rootNode) []
                    , test "Check findNodeById" <|
                        \() ->
                            Expect.notEqual (findNodeById "grandchild id" rootNode) Nothing
                    , test "Check findNodesByTitle" <|
                        \() ->
                            Expect.notEqual (findNodesByTitle "grandchild title" rootNode) []
                    ]


threeGenerationNavigationTest : Test
threeGenerationNavigationTest =
    let
        lastRoot =
            createThreeGenerations
    in
        describe "Test navigateGrandchildren" <|
            case lastRoot of
                Err message ->
                    [ test "Create children and grandchildren Nodes" <| \() -> Expect.fail message ]

                Ok rootNode ->
                    [ test "check root first-child parentId" <|
                        \() ->
                            Expect.true "First child's parent is root"
                                (isItemAttributeEqual
                                    (findNodeById "child1 id" rootNode)
                                    "parentId"
                                    "complex root id"
                                )
                    , test "check root first-child id" <|
                        \() ->
                            Expect.true "First child's id is child1 id"
                                (isItemAttributeEqual (List.head (childrenOf rootNode))
                                    "id"
                                    "child1 id"
                                    || isItemAttributeEqual (List.head (childrenOf rootNode))
                                        "id"
                                        "child2 id"
                                )
                    , test "check root child1 absence of children" <|
                        \() ->
                            case List.head (List.filter (\node -> (itemOf node).id == "child1 id") (childrenOf rootNode)) of
                                Nothing ->
                                    Expect.fail "Cannot find child1"

                                Just firstChild ->
                                    Expect.equal (List.length (childrenOf firstChild)) 0
                    , test "check root child2 presence of child" <|
                        \() ->
                            case List.head (List.filter (\node -> (itemOf node).id == "child2 id") (childrenOf rootNode)) of
                                Nothing ->
                                    Expect.fail "Cannot find child1"

                                Just firstChild ->
                                    Expect.equal (List.length (childrenOf firstChild)) 1
                    , test "check root child2 children" <|
                        \() ->
                            Expect.true "child of child2 is not grandchild"
                                (isFirstChildAttrubuteEqual
                                    (List.head (List.filter (\node -> (itemOf node).id == "child2 id") (childrenOf rootNode)))
                                    "id"
                                    "grandchild id"
                                )
                    ]


threeGenerationUpdateItemTest : Test
threeGenerationUpdateItemTest =
    let
        lastRoot =
            createThreeGenerations
    in
        describe "Test updating an item" <|
            case lastRoot of
                Err message ->
                    [ test "Create children and grandchildren Nodes" <| \() -> Expect.fail message ]

                Ok rootNode ->
                    case findNodeById "child2 id" rootNode of
                        Nothing ->
                            [ test "Finding target item" <| \() -> Expect.fail "Cannot find target" ]

                        Just targetNode ->
                            [ test "startng state" <|
                                \() -> Expect.notEqual (itemOf targetNode).description "I've been changed"
                            , test "Test updating an item" <|
                                case (updateItem (\item -> { item | description = "I've been changed" }) targetNode rootNode) of
                                    Err message ->
                                        \() -> Expect.fail message

                                    Ok updatedRootNode ->
                                        case findNodeById "child2 id" updatedRootNode of
                                            Nothing ->
                                                \() -> Expect.fail "Cannot find updated target"

                                            Just node ->
                                                \() -> Expect.equal (itemOf node).description "I've been changed"
                            ]


threeGenerationNewParentChangeParentTest : Test
threeGenerationNewParentChangeParentTest =
    let
        lastRoot =
            createThreeGenerations
    in
        describe "Test changing an item's parent" <|
            case lastRoot of
                Err message ->
                    [ test "Create children and grandchildren Nodes" <| \() -> Expect.fail message ]

                Ok rootNode ->
                    case findNodeById "grandchild id" rootNode of
                        Nothing ->
                            [ test "Finding target item" <| \() -> Expect.fail "Cannot find target" ]

                        Just targetNode ->
                            [ test "Check the new parent" <|
                                case (changeParent targetNode "child1 id" rootNode) of
                                    Err message ->
                                        \() -> Expect.fail message

                                    Ok updatedRootNode ->
                                        case findNodeById "child1 id" updatedRootNode of
                                            Nothing ->
                                                \() -> Expect.fail "Cannot find new parent"

                                            Just newParent ->
                                                \() -> Expect.equal (List.length (childrenOf newParent)) 1
                            ]


threeGenerationOldParentChangeParentTest : Test
threeGenerationOldParentChangeParentTest =
    let
        lastRoot =
            createThreeGenerations
    in
        describe "Test changing an item's parent" <|
            case lastRoot of
                Err message ->
                    [ test "Create children and grandchildren Nodes" <| \() -> Expect.fail message ]

                Ok rootNode ->
                    case findNodeById "grandchild id" rootNode of
                        Nothing ->
                            [ test "Finding target item" <| \() -> Expect.fail "Cannot find target" ]

                        Just targetNode ->
                            [ test "Check the old parent" <|
                                case (changeParent targetNode "child1 id" rootNode) of
                                    Err message ->
                                        \() -> Expect.fail message

                                    Ok updatedRootNode ->
                                        case findNodeById "child2 id" updatedRootNode of
                                            Nothing ->
                                                \() -> Expect.fail "Cannot find old parent"

                                            Just newParent ->
                                                \() -> Expect.equal (List.length (childrenOf newParent)) 0
                            ]


threeGenerationParentIdChangeParentTest : Test
threeGenerationParentIdChangeParentTest =
    let
        lastRoot =
            createThreeGenerations
    in
        describe "Test changing an item's parent" <|
            case lastRoot of
                Err message ->
                    [ test "Create children and grandchildren Nodes" <| \() -> Expect.fail message ]

                Ok rootNode ->
                    case findNodeById "grandchild id" rootNode of
                        Nothing ->
                            [ test "Finding target item" <| \() -> Expect.fail "Cannot find target" ]

                        Just targetNode ->
                            [ test "Check the item parentId" <|
                                case (changeParent targetNode "child1 id" rootNode) of
                                    Err message ->
                                        \() -> Expect.fail message

                                    Ok updatedRootNode ->
                                        \() ->
                                            Expect.true "The item should have the new parent's id"
                                                (isFirstChildAttrubuteEqual
                                                    (findNodeById "child1 id" updatedRootNode)
                                                    "parentId"
                                                    "child1 id"
                                                )
                            ]


setupChangeParent : Result String RootNode
setupChangeParent =
    let
        lastRoot =
            createMoreThreeGenerations
    in
        case lastRoot of
            Err message ->
                Err message

            Ok rootNode ->
                case findNodeById "grandchild id" rootNode of
                    Nothing ->
                        Err "Cannot find target \"grandchild id\""

                    Just targetNode ->
                        changeParent targetNode "child1 id" rootNode


threeGenerationChangeParentComplexTest : Test
threeGenerationChangeParentComplexTest =
    let
        lastRoot =
            setupChangeParent
    in
        describe "Test changing an item's parent into existing children" <|
            case lastRoot of
                Err message ->
                    [ test "Create children and grandchildren Nodes" <| \() -> Expect.fail message ]

                Ok rootNode ->
                    [ test "checking new parent" <|
                        case (findNodeById "child1 id" rootNode) of
                            Nothing ->
                                \() -> Expect.fail "Cannot find the item's new parent"

                            Just newParent ->
                                \() -> Expect.equal (List.length (childrenOf newParent)) 3
                    , test "Check the old parent" <|
                        case findNodeById "child2 id" rootNode of
                            Nothing ->
                                \() -> Expect.fail "Cannot find the item's old parent"

                            Just oldParent ->
                                \() -> Expect.equal (List.length (childrenOf oldParent)) 1
                    , test "Check the item parentId" <|
                        case findNodeById "grandchild id" rootNode of
                            Nothing ->
                                \() -> Expect.fail "Cannot find target after moving Item"

                            Just foundNode ->
                                \() -> Expect.equal (itemOf foundNode).parentId "child1 id"
                    , test "Check the item children" <|
                        case findNodeById "grandchild id" rootNode of
                            Nothing ->
                                \() -> Expect.fail "Cannot find target after moving Item"

                            Just foundNode ->
                                \() -> Expect.equal (List.length (childrenOf foundNode)) 1
                    ]


threeGenerationDeleteLeafItemTest : Test
threeGenerationDeleteLeafItemTest =
    let
        lastRoot =
            createMoreThreeGenerations
    in
        describe "Test deleting a leaf item" <|
            case lastRoot of
                Err message ->
                    [ test "Create children and grandchildren Nodes" <| \() -> Expect.fail message ]

                Ok rootNode ->
                    [ test "Search for the item" <|
                        case findNodeById "grandchild id" rootNode of
                            Nothing ->
                                \() -> Expect.fail "Cannot find target item"

                            Just targetItem ->
                                case (deleteChild targetItem rootNode) of
                                    Err message ->
                                        \() -> Expect.fail message

                                    Ok updatedRootNode ->
                                        \() -> Expect.equal (findNodeById "grandchild id" updatedRootNode) Nothing
                    ]


threeGenerationDeleteBranchItemTest : Test
threeGenerationDeleteBranchItemTest =
    let
        lastRoot =
            createMoreThreeGenerations
    in
        describe "Test deleting a brach item (i.e. with children)" <|
            case lastRoot of
                Err message ->
                    [ test "Create children and grandchildren Nodes" <| \() -> Expect.fail message ]

                Ok rootNode ->
                    [ test "Search for the item" <|
                        case findNodeById "child2 id" rootNode of
                            Nothing ->
                                \() -> Expect.fail "Cannot find target item"

                            Just targetItem ->
                                \() ->
                                    case (deleteChild targetItem rootNode) of
                                        Err message ->
                                            Expect.fail message

                                        Ok updatedRootNode ->
                                            Expect.equal (findNodeById "child2 id" updatedRootNode) Nothing
                    ]


threeGenerationOldParentDeleteLeafItemTest : Test
threeGenerationOldParentDeleteLeafItemTest =
    let
        lastRoot =
            createMoreThreeGenerations
    in
        describe "Test deleting an item" <|
            case lastRoot of
                Err message ->
                    [ test "Create children and grandchildren Nodes" <| \() -> Expect.fail message ]

                Ok rootNode ->
                    [ test "Check the old parent" <|
                        \() ->
                            case findNodeById "grandchild id" rootNode of
                                Nothing ->
                                    Expect.fail "Cannot find target item"

                                Just targetItem ->
                                    case (deleteChild targetItem rootNode) of
                                        Err message ->
                                            Expect.fail message

                                        Ok updatedRootNode ->
                                            case (findNodeById "child2 id" updatedRootNode) of
                                                Nothing ->
                                                    Expect.fail "Cannot find the deleted item's parent"

                                                Just oldParent ->
                                                    Expect.equal (List.length (childrenOf oldParent)) 1
                    ]


threeGenerationDeleteItemCheckNewParentTest : Test
threeGenerationDeleteItemCheckNewParentTest =
    let
        lastRoot =
            createMoreThreeGenerations
    in
        describe "Test deleting an item" <|
            case lastRoot of
                Err message ->
                    [ test "Create children and grandchildren Nodes" <| \() -> Expect.fail message ]

                Ok rootNode ->
                    [ test "Search for the item's child" <|
                        case findNodeById "child2 id" rootNode of
                            Nothing ->
                                \() -> Expect.fail "Cannot find target item"

                            Just targetItem ->
                                \() ->
                                    case (deleteChild targetItem rootNode) of
                                        Err message ->
                                            Expect.fail message

                                        Ok updatedRootNode ->
                                            Expect.equal (findNodeById "grandchild id" updatedRootNode) Nothing
                    ]


threeGenerationDeleteItemCheckOldParentTest : Test
threeGenerationDeleteItemCheckOldParentTest =
    let
        lastRoot =
            createMoreThreeGenerations
    in
        describe "Test deleting an item" <|
            case lastRoot of
                Err message ->
                    [ test "Create children and grandchildren Nodes" <| \() -> Expect.fail message ]

                Ok rootNode ->
                    [ test "Check the old parent" <|
                        case findNodeById "child2 id" rootNode of
                            Nothing ->
                                \() -> Expect.fail "Cannot find target item"

                            Just targetItem ->
                                case (deleteChild targetItem rootNode) of
                                    Err message ->
                                        \() -> Expect.fail message

                                    Ok updatedRootNode ->
                                        case (findNodeById (itemOf targetItem).parentId updatedRootNode) of
                                            Nothing ->
                                                \() -> Expect.fail "Cannot find the deleted item's parent"

                                            Just oldParent ->
                                                \() -> Expect.equal (List.length (childrenOf oldParent)) 1
                    ]


threeGenerationNegativeTest : Test
threeGenerationNegativeTest =
    let
        lastRoot =
            createThreeGenerations
    in
        case lastRoot of
            Err message ->
                describe "Negative tests" <|
                    [ test "Create children and grandchildren Nodes" <|
                        \() -> Expect.fail message
                    ]

            Ok rootNode ->
                describe "Negative tests" <|
                    [ test "Negative check findNodeById" <|
                        \() ->
                            Expect.equal (findNodeById "XXXX" rootNode) Nothing
                    , test "Negative test findNodeByTitle" <|
                        \() ->
                            Expect.equal (findNodesByTitle "XXXXXX" rootNode) []
                    , test "negative createChild" <|
                        \() ->
                            Expect.true "negative createChild duplicate id"
                                (isResultErr
                                    (createChild "grandchild id" "complex root id" "Beep" "Bop" rootNode)
                                )
                    , test "negative createChild" <|
                        \() ->
                            Expect.true "negative createChild non-existent parent id"
                                (isResultErr
                                    (createChild "new id" "non-existent id" "Beep" "Bop" rootNode)
                                )
                    , test "negative changeParent for new parent id" <|
                        \() ->
                            case findNodeById "grandchild id" rootNode of
                                Nothing ->
                                    Expect.fail "Cannot find target item"

                                Just targetItem ->
                                    Expect.true "negative updateItem non-existent new parent id"
                                        (isResultErr
                                            (changeParent targetItem "non-existent id" rootNode)
                                        )
                    , test "positive changeParent for previous parent id" <|
                        \() ->
                            case findNodeById "grandchild id" rootNode of
                                Nothing ->
                                    Expect.fail "Cannot find target item"

                                Just targetItem ->
                                    Expect.false "positive updateItem non-existent new parent id"
                                        (isResultErr
                                            (changeParent targetItem "child2 id" rootNode)
                                        )
                    , test "negative changeParent for the same id for item and parent" <|
                        \() ->
                            case findNodeById "grandchild id" rootNode of
                                Nothing ->
                                    Expect.fail "Cannot find target item"

                                Just targetItem ->
                                    Expect.true "negative changeParent for the same id for item and parent"
                                        (isResultErr
                                            (changeParent targetItem "grandchild id" rootNode)
                                        )
                    , test "negative isItemMember" <|
                        \() -> Expect.false "isItemMember \"XXXXX\"" (isItemMember "XXXXX" rootNode)
                    ]


createDuplicateChildren : Test
createDuplicateChildren =
    let
        firstRoot =
            Node.createRoot "root id for duplicate children" "root title" "is a root"

        secondRoot =
            Node.createChildren
                [ { id = "child1 id"
                  , parentId = "root id for duplicate children"
                  , title = "child1 title"
                  , description = "child1 description"
                  }
                , { id = "child1 id"
                  , parentId = "root id for duplicate children"
                  , title = "child2 title"
                  , description = "child2 description"
                  }
                ]
                firstRoot
    in
        case secondRoot of
            Err message ->
                describe "createDuplicateChildren" <|
                    [ test "Negative createDuplicateChildren" <|
                        \() ->
                            Expect.true "Duplicate child rejected" True
                    ]

            Ok _ ->
                describe "createDuplicateChildren" <|
                    [ test "Negative createDuplicateChildren" <|
                        \() ->
                            Expect.fail "duplicate children allowed"
                    ]


isItemAttributeEqual : Maybe (Node Item) -> String -> String -> Bool
isItemAttributeEqual maybeNode attributeName value =
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


isFirstChildAttrubuteEqual : Maybe (Node Item) -> String -> String -> Bool
isFirstChildAttrubuteEqual maybeNode attributeName value =
    case maybeNode of
        Nothing ->
            False

        Just node ->
            isItemAttributeEqual (List.head (childrenOf node)) attributeName value


isResultErr : Result a b -> Bool
isResultErr result =
    case result of
        Err _ ->
            True

        Ok _ ->
            False
