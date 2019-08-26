module BinarySearchTrees(Tree(Null), size, member, insert, toList, eq, treeMin, delete) where


  -- Binary search trees

data Tree = Null | Node Tree Integer Tree deriving Show

size :: Tree -> Integer
size Null = 0
size (Node x _ y) = 1 + (size x) + (size y)

member :: Integer -> Tree -> Bool
member i Null = False
member i (Node a b c) =  if i == b
                         then True
                         else (member i a) || (member i c)

insert :: Integer -> Tree -> Tree
insert i Null = Node Null i Null
insert i (Node a b c)  =
       if i == b
       then (Node a b c)
       else if i < b
            then (Node (insert i a) b c)
            else if i > b
                 then (Node a b (insert i c))
                 else Null


toList :: Tree -> [Integer]
toList Null = []
toList (Node a b c) = (toList a) ++ [b] ++ (toList c)



eq :: Tree -> Tree -> Bool
eq Null Null = True
eq (Node a b c) (Node d e f) = (eq a d) && (b == e) && (eq c f)
eq _ _ = False





treeMin :: Tree -> Maybe Integer
treeMin Null = Nothing
treeMin (Node Null b _) = Just b
treeMin (Node a _ _) = treeMin a


-- private things


-- Delete a Integer from a tree

deleteMin :: Tree -> Tree
deleteMin  Null = Null
deleteMin  (Node Null a Null)  = Null
deleteMin  (Node a b c) = Node (deleteMin a) b c




delete :: Integer -> Tree -> Tree
delete i Null = Null

delete i (Node a b c) =
       if i < b
       then Node (delete i a) b c
       else if i > b
            then Node a b (delete i c)

            else if (eq a Null)
                 then c
                 else if (eq c Null)
                      then a
                      else Node a (head(toList c)) (deleteMin c)
