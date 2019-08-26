module IndexTree(IndexTree(Null), insert, lookupKey, member, keys, pairs, invert) where  -- DO NOT EDIT THIS LINE



-- Index Table and Inverted Index Table

-- For this problem, you must develop a variation of a dictionary
-- called an index table (or sometimes just "index"); this is exactly
-- the same as a dictionary, except that it must have a list of values
-- associated with it instead of just a single value. Such a data
-- structure is very useful in a wide variety of applications, such
-- as concordances.
-- In particular, you can "invert" such a tree to create a reverse index.



data IndexTree k v = Null | Node (IndexTree k v) k [v] (IndexTree k v)  deriving Show

-- insert attempts to put a key and value in the index;
--     if the key is not there, it adds key and [value];
--     if key is there, but value not, it adds value to list of values;
--     if key and value there, does nothing

insert :: Ord k => Ord v => k -> v -> (IndexTree k v) -> (IndexTree k v)
insert k v Null = Node Null k [v] Null
insert k v (Node left key val right) = if k < key
                                         then Node (insert k v left) key val right
                                         else if k > key
                                              then Node left key val (insert k v right)
                                              else if (k == key) && (elem v val)
                                                   then Node left key val right
                                                   else Node left key (val ++ [v]) right


-- lookup returns list of values associated with a key, in any order, no repeates

lookupKey :: Ord k => k -> IndexTree k v -> [v]
lookupKey k Null = []
lookupKey k (Node left key val right) = if k < key
                                        then lookupKey k left
                                        else if k > key
                                             then lookupKey k right
                                             else val

-- member returns True if key and value in the tree

member :: Ord k => Eq v => k -> v -> (IndexTree k v) -> Bool
member k v Null = False
member k v (Node left key val right) = if k < key
                                        then member k v left
                                        else if k > key
                                             then member k v right
                                             else if (key == k) && (elem v val)
                                                  then True
                                                  else False


-- keys returns list of keys in the tree, in sorted order least to greatest
empty :: Eq k => (IndexTree k v) -> Bool
empty Null = True
empty _ = False
keys :: Ord k => (IndexTree k v) -> [k]
keys Null = []
keys (Node left key val right) = if (empty left)
                                 then [key] ++ (keys right)
                                 else if (empty right)
                                      then (keys left) ++ [key]
                                      else (keys left) ++ [key] ++ (keys right)


-- pairs returns a list of (key,value) pairs for each in the index tree in any order
pairs :: (IndexTree k v)  -> [(k,v)]
pairs Null = []
pairs (Node Null key val Null) = map (\a -> (key, a)) val
pairs (Node left key val right) = (map (\a -> (key, a)) val) ++ (pairs left) ++ (pairs right)


-- invert takes an index tree, and creates a new index tree with the values as keys
-- and keys as values; note that since we don't want to use state, we'll do this
-- using list processing only

invert :: Ord k => Ord v => (IndexTree k v)  -> (IndexTree v k)
invert tree = invertHelper (flip' tree) Null



-- when are 2 IndexTrees equal?
instance (Ord k, Eq v) => Eq (IndexTree k v) where
  Null == Null = True
  treeX == treeY = checkEq treeX (pairs treeY)

checkEq :: Ord k => Eq v => IndexTree k v -> [(k,v)] -> Bool
checkEq _ [] = True
checkEq Null _ = False
checkEq treex ((k,v):xs) = if (member k v treex)
                           then checkEq treex xs
                           else False


instance Functor (IndexTree k) where
--  fmap :: (a -> b) -> (IndexTree k a) -> (IndexTree k b)
  fmap _ Null = Null
  fmap f (Node left key value right) = Node (fmap f left) (key) (map (\a-> f a) value) (fmap f right)

-- helper functions you may choose to implement
flip' :: (IndexTree k v)  -> [(v,k)]
flip' Null = []
flip' (Node Null key val Null) = map (\a -> (a, key)) val
flip' (Node left key val right) = (flip' left) ++ (map (\a -> (a, key)) val) ++ (flip' right)

invertHelper :: Ord k => Ord v =>  [(k,v)] -> (IndexTree k v) -> (IndexTree k v)
invertHelper list tree2 = foldr invertHelper2 tree2 list
invertHelper2 :: Ord k => Ord v =>  (k,v) -> (IndexTree k v) -> (IndexTree k v)
invertHelper2 (k,v) tree3 = insert k v tree3

-- an example


t = invertHelper [( "a","1"), ("b","2"),("c","3"),("a","3"),("b","5")] Null

tinv = invert t
