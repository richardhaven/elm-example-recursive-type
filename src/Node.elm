module Node
    exposing
        ( Item
        , ItemId
        , Node
        , ItemList
        , RootNode
        , itemOf
        , childrenOf
        , createRoot
        , createChildren
        , createChild
        , updateItem
        , changeParent
        , deleteChild
        , findNodeById
        , isItemMember
        , findNodesByTitle
        , nestedItemMap
        , flatList
        )

{-|
    This is a sample system for modeling recursive, hierarchical elements

    The definition of Node comes from Chad Gilbert in an answer on Stack Overflow
    on 5 October 2016

            type Node a = Node a (List (Node a))

            type alias Item = { id: String, text: String }

            map : (a -> b) -> Node a -> Node b
            map f (Node item children) =
                Node (f item) (List.map (map f) children)

    I fleshed out the basic support methods as well as the tests as an example.

    When changing any item or adding or removing any child, one must regenerate the entire tree
    because that's all the caller can deal with: replacing the root node.

        update msg model =
            case msg of
                CreateChild parentId title description ->
                    {model | rootNode = Node.createChild (findNextId model.rootNode) parentId title description model.rootNode }

    If one's model allows workinig with a lower branch as a sub-set (e.g. "headNode")

                UpdateChildTitle id newTitle ->
                    updateRootAndHead model (Node.updateChild (\item -> {item | title = newTitle} id  model.rootNode)

        updateRootAndHead model newRoot =
            let
                newHead = Maybe.withDefault newRoot findNodeById itemof(model.headNode).id newRoot
            in
                {model | rootNode = newRoot, headNode = newHead}

-}


{-|
    Note that the Item record can have anything in it, and does not need to have the parentId
    as long as the deleteChild gets the parent id somehow
-}
type alias ItemId =
    String


type alias Item =
    { id : ItemId
    , parentId : ItemId
    , title : String
    , description : String
    }


type Node a
    = Node a (List (Node a))


type alias ItemList =
    List (Node Item)


type alias RootNode =
    Node Item


itemOf : Node Item -> Item
itemOf (Node item _) =
    item


childrenOf : Node Item -> ItemList
childrenOf (Node item children) =
    children


{-| Store this value in the model, perhaps by making it part of the init

init : (Model, Cmd Msg)
init =
    ( {foo = "", rootNode = (createRoot 1 "root" "the root node")}, Cmd.none )
-}
createRoot : ItemId -> String -> String -> RootNode
createRoot rootId rootName rootDescription =
    Node
        { id = rootId
        , parentId = ""
        , title = rootName
        , description = rootDescription
        }
        []


{-| Creates new Items and returns the final rootNode. If any child creation fails, no rollback or undo needed
    let
        newRoot = createChildren [{"43" "7" "new child" "this is a new child Item"},
                                  {"48", "43", "grandchild", ""}]
                                  model.rootNode
    in
        case newRoot of
            Err message ->
                {model | errorMessage = message}
            Ok newRoot ->
                {model | rootNode = newRoot, errorMessage = ""}
-}
createChildren : List Item -> RootNode -> Result String RootNode
createChildren childAttributesList root =
    List.foldl
        (\attributes previousRoot ->
            case previousRoot of
                Err message ->
                    Err message

                Ok root ->
                    createChild attributes.id attributes.parentId attributes.title attributes.description root
        )
        (Ok root)
        childAttributesList


{-| Creates a new Item and returns the updated rootNode
    let
        newRoot = createChild 43 7 "new child" "this is a new child Item" model.rootNode
    in
        case newRoot of
            Err message ->
                {model | errorMessage = message}
            Ok newRoot ->
                {model | rootNode = newRoot, errorMessage = ""}
-}
createChild : ItemId -> ItemId -> String -> String -> RootNode -> Result String RootNode
createChild anItemId aParentId aTitle aDescription root =
    let
        newItem =
            Node
                { id = anItemId
                , parentId = aParentId
                , title = aTitle
                , description = aDescription
                }
                []
    in
        case getValidationErrors newItem root of
            Just errorMessage ->
                Err errorMessage

            Nothing ->
                Ok
                    (nestedNodeMap
                        (\aNode ->
                            if (itemOf aNode).id == aParentId then
                                Node (itemOf aNode) (newItem :: (childrenOf aNode))
                            else
                                aNode
                        )
                        root
                    )


getValidationErrors : Node Item -> RootNode -> Maybe String
getValidationErrors newChild root =
    if (findNodeById (itemOf newChild).id root) /= Nothing then
        Just ("Item id " ++ (toString (itemOf newChild).id) ++ " already exists")
    else if (findNodeById (itemOf newChild).parentId root) == Nothing then
        Just ("Parent id " ++ (toString (itemOf newChild).parentId) ++ " not found for " ++ (toString (itemOf newChild).id))
    else
        Nothing


{-| Removes the indicated Item and returns the new rootNode

    {model | rootNode = deleteChild doomedItem model.rootNode}
-}
deleteChild : ItemId -> RootNode -> Result String RootNode
deleteChild targetId root =
    let
        targetNodeMaybe =
            findNodeById targetId root
    in
        case targetNodeMaybe of
            Nothing ->
                Err ("Cannot find target item id " ++ (toString targetId))

            Just (Node targetItem _) ->
                Ok
                    (nestedNodeMap
                        (\(Node item children) ->
                            if item.id == targetItem.parentId then
                                Node item (List.filter (\(Node anItem aChildren) -> anItem.id /= targetId) children)
                            else
                                Node item children
                        )
                        root
                    )


{-| Updates the indicated Item and returns the new rootNode

    {model | rootNode = updateChild (\item -> {item | title = "Fred") "child1223" model.rootNode}

    This wrapper around nestedItemMap prevents accidentally forgetting to test for a specific
    Item id and modifying every Item.

    We return a Result even though we could return the new RootNode for consistency with the other methods

    Do NOT change the parentId here; use changeParent
-}
updateItem : (Item -> Item) -> ItemId -> RootNode -> Result String RootNode
updateItem aFunction targetId root =
    let
        targetNodeMaybe =
            findNodeById targetId root
    in
        case targetNodeMaybe of
            Nothing ->
                Err ("Cannot find target item id " ++ (toString targetId))

            Just (Node targetItem _) ->
                Ok
                    (nestedItemMap
                        (\item ->
                            let
                                oldParentId =
                                    item.parentId

                                oldId =
                                    item.id
                            in
                                if item.id == targetItem.id then
                                    (\anItem -> { anItem | id = oldId, parentId = oldParentId }) <| (aFunction item)
                                else
                                    item
                        )
                        root
                    )


{-|
    Changes the parent of an item and returns the new rootNode or an error message

    case of updateChild childId newParentId model.root of
        Err message ->
            {model | errorMessage = message}

        Ok newRootNode ->
            {model | errorMessage = "", root = newRootNode}

-}
changeParent : ItemId -> ItemId -> RootNode -> Result String RootNode
changeParent targetId newParentId root =
    let
        targetNodeMaybe =
            findNodeById targetId root
    in
        case targetNodeMaybe of
            Nothing ->
                Err ("Cannot find target item id " ++ (toString targetId))

            Just (Node targetItem targetChildren) ->
                if targetItem.id == newParentId then
                    Err "An item must not have itself as its parent"
                else if targetItem.parentId == newParentId then
                    Ok root
                else if (findNodeById newParentId root) == Nothing then
                    Err "New parent not found"
                else
                    Ok (setNodeParent (Node targetItem targetChildren) newParentId root)


setNodeParent : Node Item -> ItemId -> RootNode -> RootNode
setNodeParent targetNode newParentId root =
    let
        targetItem =
            itemOf targetNode

        updatedItem =
            { targetItem | parentId = newParentId }

        updatedNode =
            Node updatedItem (childrenOf targetNode)
    in
        nestedNodeMap
            (\(Node anItem aChildren) ->
                --  we remove the item from the previous parent
                if anItem.id == targetItem.parentId then
                    Node anItem (List.filter (\(Node childItem _) -> childItem.id /= targetItem.id) aChildren)
                    --  and add it to the new parent's children (and update its .parentId)
                else if anItem.id == newParentId then
                    Node anItem (updatedNode :: aChildren)
                else
                    Node anItem aChildren
            )
            root


isItemMember : ItemId -> RootNode -> Bool
isItemMember targetId root =
    List.foldl
        (\node isFound ->
            if isFound then
                True
            else
                (itemOf node).id == targetId
        )
        False
        (flatList root)


{-| Returns a (flat) List of Items matching the title string
        this can be an empty List

    {model | searchResults = findNodesByTitle model.searchText model.rootNode}
-}
findNodesByTitle : String -> RootNode -> ItemList
findNodesByTitle title root =
    let
        lowerTitle =
            String.toLower title
    in
        if String.length title == 0 then
            []
        else
            flatItemFilter (\anItem -> String.contains lowerTitle (String.toLower anItem.title)) root


{-| Returns a Maybe (Node Item) with the found Item or Nothing
    let
        foundItem = findNodeById itemIdToFind model.rootNode
    in
        case foundItem of
            Nothing ->
                {model | searchResults = []}
            Just item ->
                {model | searchResults = [item]}
-}
findNodeById : ItemId -> RootNode -> Maybe (Node Item)
findNodeById targetId root =
    let
        matches =
            List.filter (\aNode -> (itemOf aNode).id == targetId) (flatList root)
    in
        List.head matches


{-| returns a single, non-nested List (like List.concatMap) with
    all Items from every generation
-}
flatList : RootNode -> ItemList
flatList root =
    let
        rest =
            List.concatMap (\child -> flatList child) (childrenOf root)
    in
        root :: rest


flatItemMap : (Item -> Item) -> RootNode -> ItemList
flatItemMap aFunction root =
    List.map (\child -> Node (aFunction (itemOf child)) (childrenOf root)) (flatList root)


flatNodeMap : (Node Item -> Node Item) -> Node Item -> ItemList
flatNodeMap aFunction root =
    List.map (\aNode -> aFunction aNode) (flatList root)


flatItemFilter : (Item -> Bool) -> RootNode -> ItemList
flatItemFilter aFunction root =
    List.filter (\aNode -> (aFunction (itemOf aNode))) (flatList root)


flatNodeFilter : (Node Item -> Bool) -> RootNode -> ItemList
flatNodeFilter aFunction root =
    List.filter (\aNode -> aFunction aNode) (flatList root)


{-| returns a new version of the Node parameter (e.g. rootNode)
    with all nodes' items coming through the passed function
-}
nestedItemMap : (Item -> Item) -> RootNode -> RootNode
nestedItemMap aFunction root =
    let
        newChildren =
            List.map (\child -> nestedItemMap aFunction child) (childrenOf root)
    in
        Node (aFunction (itemOf root)) newChildren


{-| returns a new version of the Node parameter (e.g. rootNode)
    with all nodes coming through the passed function

    createChild and deleteChild use this to alter the children List
    of the child's parent
-}
nestedNodeMap : (Node Item -> Node Item) -> RootNode -> RootNode
nestedNodeMap aFunction root =
    let
        newChildren =
            List.map (\child -> nestedNodeMap aFunction child) (childrenOf root)
    in
        aFunction (Node (itemOf root) newChildren)


{-| returns a new version of the Node parameter (e.g. rootNode)
    as a Maybe with only those nodes approved by the function.

    The result must be a Maybe because even the root might be omitted.

    Note that any omitted Node's children are also omitted even if they
    would have been approved.
-}
nestedItemFilter : (Item -> Bool) -> RootNode -> Maybe (Node Item)
nestedItemFilter aFunction root =
    let
        newChildren =
            List.filterMap (\child -> nestedItemFilter aFunction child) (childrenOf root)
    in
        if aFunction (itemOf root) then
            Just (Node (itemOf root) newChildren)
        else
            Nothing
