module IndexTreeBad(IndexTree(),empty, insert, Compare(LessThan, EqualTo, GreaterThan),Ordering(Ordering) ) where
import Prelude hiding (lookup, elem, Ordering)

data Compare = LessThan | EqualTo | GreaterThan

data Ordering a = Ordering (a -> a -> Compare)

intOrdering :: Ordering Integer
intOrdering = Ordering helpr

helpr :: Integer -> Integer -> Compare
helpr x y | x == y = EqualTo
          | x < y = LessThan
          | otherwise = GreaterThan



-- Example 2: Index Table and Inverted Index Table

-- For this problem, you must develop a variation of a dictionary
-- called an index table (or sometimes just "index"); this is exactly
-- the same as a dictionary, except that it must have a list of values
-- associated with it instead of just a single value. Such a data
-- structure is very useful in a wide variety of applications, such
-- as concordances.
-- In particular, you can "invert" such a tree to create a reverse index.



data IndexTree k v = IndexTree  (Ordering k) (Ordering v) (IndexTreeInner k v)



data IndexTreeInner k v = Null | Node (IndexTreeInner k v) k [v] (IndexTreeInner k v)


-- lab
empty ::  (Ordering k) -> (Ordering v) -> IndexTree k v
empty ok ov = IndexTree ok ov Null


-- helper functions that might be helpful
elem :: Ordering a -> a -> [a] -> Bool
elem oa a [] = False
elem (Ordering oa) a (la:las) = case (oa a la) of LessThan -> (elem (Ordering oa) a las)
                                                  GreaterThan -> (elem (Ordering oa) a las)
                                                  EqualTo -> True


insertHelper :: (Ordering k) -> (Ordering v) -> k -> v -> (IndexTreeInner k v ) -> (IndexTreeInner k v )
insertHelper ok ov k v Null = Node Null k [v] Null
insertHelper (Ordering ok) ov k v (Node left key val right) = case (ok k key) of
                                                              LessThan -> Node (insertHelper (Ordering ok) ov k v left) key val right
                                                              GreaterThan -> Node left key val (insertHelper (Ordering ok) ov k v right)
                                                              EqualTo -> if elem ov v val
                                                                         then Node left key val right
                                                                         else Node left key (val ++ [v]) right

-- insert attempts to put a key and value in the index;
--     if the key is not there, it adds key and [value];
--     if key is there, but value not, it adds value to list of values;
--     if key and value there, does nothing


insert :: k -> v -> (IndexTree k v ) -> (IndexTree k v )
insert k v (IndexTree ok ov innerTree) = IndexTree ok ov (insertHelper ok ov k v innerTree)


-- sooo much work...
