module Outside where
import Set
import BinarySearchTrees

-- Set of 2,4,8,16,32,64
exampleSet :: Set
exampleSet = Set.insert 2 (Set.insert 4 (Set.insert 8 (Set.insert 16 (Set.insert 32 (Set.insert 64 (empty))))))

-- when are 2 sets equal?
eqSet :: Set -> Set -> Bool
eqSet [] [] = True
eqSet x y = if (Set.isSubSet x y) && (Set.isSubSet y x)
            then True
            else False


-- Tree of 2,4,8,16,32,64
exampleBst :: Tree
exampleBst = BinarySearchTrees.insert 2 (BinarySearchTrees.insert 4 (BinarySearchTrees.insert 8 (BinarySearchTrees.insert 16 (BinarySearchTrees.insert 32 (BinarySearchTrees.insert 64 Null)))))

-- contains only even
onlyHasEven :: Tree -> Bool
onlyHasEven Null = False
onlyHasEven t = if even (head (BinarySearchTrees.toList t)) && ((tail (BinarySearchTrees.toList t)) == [])
                then True
                else if even (head (BinarySearchTrees.toList t))
                     then onlyHasEven (BinarySearchTrees.delete (head (BinarySearchTrees.toList t)) t)
                     else False
