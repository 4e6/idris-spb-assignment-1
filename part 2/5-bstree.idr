data BSTree : (a: Type) -> Type where
  Empty : Ord a => BSTree a
  Node  : Ord a => (left : BSTree a) -> (val : a) ->
                  (right : BSTree a) -> BSTree a

%name BSTree t, t1

insert : a -> BSTree a -> BSTree a
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) =
     case compare x val of
        LT => Node (insert x left) val right
        EQ => orig
        GT => Node left val (insert x right)

||| Inserts elements of a list into a binary search tree
listToTree : Ord a => List a -> BSTree a
listToTree = foldl (flip insert) Empty

||| Creates list from the elements of BSTree
treeToList : BSTree a -> List a
treeToList Empty = []
treeToList (Node left val right) = treeToList left ++ [val] ++ treeToList right

test_bstree_sorted : Ord a => List a -> Bool
test_bstree_sorted = sorted . treeToList . listToTree

test_bstree : Bool
test_bstree = test_bstree_sorted [1,6,2,3,7]
