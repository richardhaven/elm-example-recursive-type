module ThreeGenerationTest exposing (..)

import Expect
import Test exposing (..)


-- import Fuzz exposing (list, int, tuple, string)

import Node exposing (..)


threeGenerationTest : Test
threeGenerationTest =
    describe "Test three generations" <|
        [ threeGenerationCreationTest
        , threeGenerationNavigationTest
        , threeGenerationUpdateItemTest
        , threeGenerationChangeParentTest
        , threeGenerationChangeParentComplexTest
        , threeGenerationDeleteItemTest
        , threeGenerationNegativeTest
        ]


createThreeGenerations : Result String (Node Item)
createThreeGenerations =
    let
        firstRoot =
            Node.createRoot "root id" "root" "is a root"
    in
        Node.createChildren
            [ { id = "child1 id"
              , parentId = "root"
              , title = "child1 title"
              , description = "child1 description"
              }
            , { id = "child2 id"
              , parentId = "root id"
              , title = "child2 title"
              , description = "child2 description"
              }
            , { id = "grandchild id"
              , parentId = "child2 id"
              , title = "grandchild title"
              , description = "grandchild description"
              }
            ]
            firstRoot


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
                describe "Test createChildren" <|
                    [ test "createChild id" <|
                        \() ->
                            Expect.notEqual (findNodeById "child1 id" rootNode) Nothing
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
                    , test "Negative check findNodeById" <|
                        \() ->
                            Expect.equal (findNodeById "XXXX" rootNode) Nothing
                    , test "Negative test findNodeByTitle" <|
                        \() ->
                            Expect.equal (findNodesByTitle "XXXXXX" rootNode) []
                    ]


threeGenerationNavigationTest : Test
threeGenerationNavigationTest =
    let
        lastRoot =
            createThreeGenerations
    in
        case lastRoot of
            Err message ->
                describe "Test createGrandchildren" <| [ test "Create children and grandchildren Nodes" <| \() -> Expect.fail message ]

            Ok rootNode ->
                describe "Test createChildren" <|
                    [ test "check root first-child parentId" <|
                        \() ->
                            Expect.true "First child's parent is root"
                                (isItemAttributeEqual
                                    (findNodeById "child1 id" rootNode)
                                    "parentId"
                                    "root id"
                                )
                    , test "check root first-child id" <|
                        \() ->
                            Expect.true "First child's parent is root"
                                (isItemAttributeEqual (List.head (childrenOf rootNode))
                                    "id"
                                    "child1 id"
                                )
                    , test "check root first child children" <|
                        \() ->
                            case List.head (childrenOf rootNode) of
                                Nothing ->
                                    Expect.fail "Root children is empty"

                                Just firstChild ->
                                    Expect.equal (childrenOf firstChild) []
                    , test "check root second-child children" <|
                        \() ->
                            case (List.foldl (Just >> always) Nothing (childrenOf rootNode)) of
                                Nothing ->
                                    Expect.fail "Root children is empty"

                                Just lastChild ->
                                    Expect.true "Grandchild is in child's children"
                                        (isFirstChildAttrubuteEqual
                                            (List.head (childrenOf lastChild))
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
        case lastRoot of
            Err message ->
                describe "Test updating an item" <| [ test "Create children and grandchildren Nodes" <| \() -> Expect.fail message ]

            Ok rootNode ->
                case findNodeById "child2 id" rootNode of
                    Nothing ->
                        describe "Test updating an item" <| [ test "Finding target item" <| \() -> Expect.fail "Cannot find target" ]

                    Just targetNode ->
                        describe "Test updating an item" <|
                            [ test "Test updating an item" <|
                                \() ->
                                    case (updateItem (\item -> { item | description = "I've been changed" }) targetNode rootNode) of
                                        Err message ->
                                            Expect.fail message

                                        Ok updatedRootNode ->
                                            Expect.true "Updating an item should have a new description"
                                                (isItemAttributeEqual
                                                    (findNodeById "child2 id" updatedRootNode)
                                                    "description"
                                                    "I've been changed"
                                                )
                            ]


threeGenerationChangeParentTest : Test
threeGenerationChangeParentTest =
    let
        lastRoot =
            createThreeGenerations
    in
        case lastRoot of
            Err message ->
                describe "Test changing an item's parent" <| [ test "Create children and grandchildren Nodes" <| \() -> Expect.fail message ]

            Ok rootNode ->
                case findNodeById "grandchild id" rootNode of
                    Nothing ->
                        describe "Test changing an item's parent" <| [ test "Finding target item" <| \() -> Expect.fail "Cannot find target" ]

                    Just targetNode ->
                        describe "Test changing an item's parent" <|
                            [ test "Check the new parent" <|
                                \() ->
                                    case (changeParent targetNode "child1 id" rootNode) of
                                        Err message ->
                                            Expect.fail message

                                        Ok updatedRootNode ->
                                            Expect.true "The new parent should have the child"
                                                (isFirstChildAttrubuteEqual
                                                    (findNodeById "child1 id" updatedRootNode)
                                                    "id"
                                                    "grandchild id"
                                                )
                            , test "Check the old parent" <|
                                \() ->
                                    case (changeParent targetNode "child1 id" rootNode) of
                                        Err message ->
                                            Expect.fail message

                                        Ok updatedRootNode ->
                                            case (List.foldl (Just >> always) Nothing (childrenOf updatedRootNode)) of
                                                Nothing ->
                                                    Expect.fail "Root has no children"

                                                Just secondChild ->
                                                    Expect.equal (childrenOf secondChild) []
                            , test "Check the item parentId" <|
                                \() ->
                                    case (changeParent targetNode "child1 id" rootNode) of
                                        Err message ->
                                            Expect.fail message

                                        Ok updatedRootNode ->
                                            Expect.true "The item should have the new parent's id"
                                                (isFirstChildAttrubuteEqual
                                                    (findNodeById "child1 id" updatedRootNode)
                                                    "parentId"
                                                    "child1 id"
                                                )
                            ]


threeGenerationChangeParentComplexTest : Test
threeGenerationChangeParentComplexTest =
    let
        lastRoot =
            createThreeGenerations
    in
        case lastRoot of
            Err message ->
                describe "Test changing an item's parent into existing children" <| [ test "Create children and grandchildren Nodes" <| \() -> Expect.fail message ]

            Ok rootNode ->
                case
                    (Node.createChildren
                        [ { id = "child1 child1 id", parentId = "child1 id", title = "child11 title", description = "child11 description" }
                        , { id = "child1 child2 id", parentId = "child1 id", title = "child12 title", description = "child12 description" }
                        , { id = "child2 child2", parentId = "child2 id", title = "child22 title", description = "child22 description" }
                        , { id = "grandchild child id", parentId = "grancdchild child id", title = "child211 title", description = "child211 description" }
                        ]
                        rootNode
                    )
                of
                    Err message ->
                        describe "Test changing an item's parent into existing children" <| [ test "Create additional grandchildren Nodes" <| \() -> Expect.fail message ]

                    Ok newRootNode ->
                        case findNodeById "grandchild id" newRootNode of
                            Nothing ->
                                describe "Test changing an item's parent into existing children" <|
                                    [ test "Finding target item" <|
                                        \() -> Expect.fail "Cannot find target"
                                    ]

                            Just targetNode ->
                                describe "Test changing an item's parent into existing children" <|
                                    [ test "Check the new parent" <|
                                        \() ->
                                            case (changeParent targetNode "child1 id" newRootNode) of
                                                Err message ->
                                                    Expect.fail message

                                                Ok updatedRootNode ->
                                                    case (findNodeById "child1 id" updatedRootNode) of
                                                        Nothing ->
                                                            Expect.fail "Cannot find the item's new parent"

                                                        Just oldParent ->
                                                            Expect.equal (List.length (childrenOf oldParent)) 1
                                    , test "Check the old parent" <|
                                        \() ->
                                            case (changeParent targetNode "child1 id" newRootNode) of
                                                Err message ->
                                                    Expect.fail message

                                                Ok updatedRootNode ->
                                                    case (findNodeById "child2 id" updatedRootNode) of
                                                        Nothing ->
                                                            Expect.fail "Cannot find the item's old parent"

                                                        Just newParent ->
                                                            Expect.equal (List.length (childrenOf newParent)) 3
                                    , test "Check the item parentId" <|
                                        \() ->
                                            case (changeParent targetNode "child1 id" newRootNode) of
                                                Err message ->
                                                    Expect.fail message

                                                Ok updatedRootNode ->
                                                    case findNodeById "grandchild id" updatedRootNode of
                                                        Nothing ->
                                                            Expect.fail "Cannot find target after moving Item"

                                                        Just (Node movedItem _) ->
                                                            Expect.equal movedItem.parentId "child1 id"
                                    , test "Check the item children" <|
                                        \() ->
                                            case (changeParent targetNode "child1 id" newRootNode) of
                                                Err message ->
                                                    Expect.fail message

                                                Ok updatedRootNode ->
                                                    case findNodeById "grandchild id" updatedRootNode of
                                                        Nothing ->
                                                            Expect.fail "Cannot find target after moving Item"

                                                        Just (Node movedItem movedChildren) ->
                                                            Expect.equal (List.length movedChildren) 1
                                    ]


threeGenerationDeleteItemTest : Test
threeGenerationDeleteItemTest =
    let
        lastRoot =
            createThreeGenerations
    in
        case lastRoot of
            Err message ->
                describe "Test deleting an item" <| [ test "Create children and grandchildren Nodes" <| \() -> Expect.fail message ]

            Ok rootNode ->
                describe "Test deleting an item" <|
                    [ describe "Test deleting a leaf item" <|
                        [ test "Search for the item" <|
                            case findNodeById "grandchild id" rootNode of
                                Nothing ->
                                    \() -> Expect.fail "Cannot find target item"

                                Just targetItem ->
                                    \() ->
                                        case (deleteChild targetItem rootNode) of
                                            Err message ->
                                                Expect.fail message

                                            Ok updatedRootNode ->
                                                Expect.equal (findNodeById "grandchild id" rootNode) Nothing
                        , test "Check the old parent" <|
                            \() ->
                                case findNodeById "grandchild id" rootNode of
                                    Nothing ->
                                        Expect.fail "Cannot find target item"

                                    Just targetItem ->
                                        case (deleteChild targetItem rootNode) of
                                            Err message ->
                                                Expect.fail message

                                            Ok updatedRootNode ->
                                                case (findNodeById "child2 id" rootNode) of
                                                    Nothing ->
                                                        Expect.fail "Cannot find the deleted item's parent"

                                                    Just oldParent ->
                                                        Expect.equal (List.length (childrenOf oldParent)) 0
                        ]
                    , describe "Test deleting a brach item (i.e. with children)" <|
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
                                                Expect.equal (findNodeById "child2 id" rootNode) Nothing
                        , test "Search for the item's child" <|
                            case findNodeById "child2 id" rootNode of
                                Nothing ->
                                    \() -> Expect.fail "Cannot find target item"

                                Just targetItem ->
                                    \() ->
                                        case (deleteChild targetItem rootNode) of
                                            Err message ->
                                                Expect.fail message

                                            Ok updatedRootNode ->
                                                Expect.equal (findNodeById "grandchild id" rootNode) Nothing
                        , test "Check the old parent" <|
                            \() ->
                                case findNodeById "child2 id" rootNode of
                                    Nothing ->
                                        Expect.fail "Cannot find target item"

                                    Just targetItem ->
                                        case (deleteChild targetItem rootNode) of
                                            Err message ->
                                                Expect.fail message

                                            Ok updatedRootNode ->
                                                case (findNodeById "root id" rootNode) of
                                                    Nothing ->
                                                        Expect.fail "Cannot find the deleted item's parent"

                                                    Just oldParent ->
                                                        Expect.equal (List.length (childrenOf oldParent)) 1
                        ]
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
                    [ test "negative createChild" <|
                        \() ->
                            Expect.true "negative createChild duplicate id"
                                (isResultErr
                                    (createChild "grandchild id" "root id" "Beep" "Bop" rootNode)
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
                                            (changeParent targetItem "child2  id" rootNode)
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
