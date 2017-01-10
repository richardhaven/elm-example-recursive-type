# elm-example-recursive-type
An example of a type that has a list of child-types: nested recursive inclusion

This shows how to manage the hierarchical tree and how to see what's going on

src/Node.elm is commented and tests/NodeTest.elm shows how to call the methods

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

