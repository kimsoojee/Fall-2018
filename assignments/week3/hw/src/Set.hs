module Set (Set, empty, contains, insert, isSubSet, union, intersection) where

-- sorted list

type Set = [Integer]


empty :: Set
empty = []

contains :: Set -> Integer -> Bool
contains [] _ = False
contains (start : ls) i = if i == start
                          then True
                          else if i < start
                               then False
                               else contains ls i



insert :: Integer -> Set -> Set
insert x [] = [x]
insert x (start : ls) = if x < start
                        then x : (start : ls)   -- = x : start : ls
                        else if x > start
                             then start : (insert x ls)
                             else (start : ls)




-- Functions on lists, using built-in lists

isPrefix :: Set -> Set -> Bool
isPrefix _ [] = False
isPrefix [] _ = True
isPrefix (x:xs) (y:ys) = if x == y
                         then isPrefix xs ys
                         else False

isSubSet :: Set -> Set -> Bool
-- check if first set is subset of second set
isSubSet [] _ = True
isSubSet _ [] = False
isSubSet (x:xs) (y:ys) = if x == y
                         then isSubSet xs ys
                         else if (x /= y) && (contains (y:ys) x)
                              then isSubSet xs (y:ys)
                              else False

--if x == y
  --                       then isPrefix (x:xs) (y:ys)
    --                     else isSubSet (x:xs) ys



-- find the union of two ordered lists, this is like merge but
-- removing duplicates

union :: Set -> Set -> Set
union a [] = a
union [] b = b
union (x:xs) (y:ys) = if x < y
                      then x:(union xs (y:ys))
                      else if x > y
                           then y:(union (x:xs) ys)
                           else if x == y
                                then x:(union xs ys)
                                else []



-- find the intersection of two ordered lists, i.e., the
-- common subsequence

intersection :: Set -> Set -> Set
intersection [] [] = []
intersection _ [] = []
intersection [] _ = []
intersection (x:xs) y = if (contains y x)
                        then x:(intersection xs y)
                        else intersection xs y
