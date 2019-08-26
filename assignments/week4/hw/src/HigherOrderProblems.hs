module HigherOrderProblems where

-- this forces you to work with our more annoying lists, DO NOT CHANGE THE FOLLOWING 2 LINES
import Prelude hiding (map, foldr, length, filter)
import AnnoyingLists


{-   This problem is a collection of functions exercising your
     knowledge of higher-order programming. Follow the instructions
     for each function.
     NOTE: It is EXPECTED that you will not know absolutely everything
     about some of the functions (or the list comprehensions) referenced
     below, but that you know how to use the references given on the
     class web page, or Google, to find out the relevant information
     needed to complete the problems. :-)
-}





-- (A) -- (F) Reimplement each of the following on annoying lists.

problemA xs    = [ x+1 | x <- xs ]
solutionA :: AnnoyingList Integer -> AnnoyingList Integer
solutionA annoy = map problemAHelper annoy
problemAHelper :: Integer -> Integer
problemAHelper int = int + 1


problemB xs    = [ x*2 | x <- xs, x > 2 ]
solutionB :: AnnoyingList Integer -> AnnoyingList Integer
solutionB annoy = foldr problemBHelper (fromList []) annoy
problemBHelper :: Integer -> AnnoyingList Integer -> AnnoyingList Integer
problemBHelper int annoy2 = if int > 2
                            then cons (int*2) annoy2
                            else annoy2


problemC xys   = [ 1/x | (x,_) <- xys ]
solutionC :: AnnoyingList (Integer, Integer) -> AnnoyingList Float
solutionC annoy = map problemCHelper annoy
problemCHelper :: (Integer, Integer) -> Float
problemCHelper (x, _) = 1/(fromIntegral x)


problemD xys   = [ (-x) | (x,y) <- xys, x+y < 5 ]
solutionD ::  AnnoyingList (Integer, Integer) -> AnnoyingList Integer
solutionD annoy = map problemDHelper (AnnoyingLists.filter problemDHelper2 annoy)
problemDHelper :: (Integer, Integer) -> Integer
problemDHelper (x, _) = (-1) * x
problemDHelper2 :: (Integer, Integer) -> Bool
problemDHelper2 (x, y) = if (x+y) < 5
                      then True
                      else False


problemE xs ys = [ x+y | x <- xs, y <- ys ]
solutionE ::  AnnoyingList Integer -> AnnoyingList Integer -> AnnoyingList Integer
solutionE annoyX annoyY = foldr problemEHelper (fromList []) (fromList ((zip (makeList annoyX) (makeList annoyY))))
makeList :: AnnoyingList Integer -> [Integer]
makeList annoylist = foldr makeListHelper [] annoylist
makeListHelper :: Integer -> [Integer] -> [Integer]
makeListHelper int intList = (int : intList)
problemEHelper :: (Integer, Integer) -> AnnoyingList Integer -> AnnoyingList Integer
problemEHelper (x, y) annoy = cons (x+y) annoy


-- lab answers
problemF mxs   = [ x^2 | Just x <- mxs ]
solutionF :: AnnoyingList (Maybe Integer) ->  AnnoyingList Integer
solutionF annoy = foldr problemFHelper (fromList []) annoy
problemFHelper :: (Maybe Integer) -> (AnnoyingList Integer) -> AnnoyingList Integer
problemFHelper (Just integer) annoy2 = cons (integer^2) annoy2
problemFHelper Nothing annoy2 = annoy2


-- (G) Implement length

length :: AnnoyingList a -> Integer
length annoy = sum (foldr lengthHelper [] annoy)
lengthHelper :: a -> [Integer] -> [Integer]
lengthHelper x y = [1] ++ y

-- (H) Implement append

append :: AnnoyingList a  -> AnnoyingList a -> AnnoyingList a
append annoy1 annoy2 = foldr appendHelper annoy2 annoy1
appendHelper :: a -> AnnoyingList a -> AnnoyingList a
appendHelper x y= cons x y

-- (I) longest takes a list of strings and returns the length
--     of the longest member string.

longest :: AnnoyingList (AnnoyingList Char) -> Maybe Integer
longest annoy = foldr longestHelper Nothing annoy
longestHelper :: AnnoyingList Char -> Maybe Integer -> Maybe Integer
longestHelper char Nothing = if (length char) == 0
                             then Nothing
                             else Just (length char)
longestHelper char (Just x) = if (length char > x)
                              then Just (length char)
                              else Just x

-- (J) Now define flatten which does the same thing as concat to but to a list of lists
flatten :: AnnoyingList (AnnoyingList a) -> (AnnoyingList a)
flatten annoyannoy = foldr flattenHelper (fromList []) annoyannoy
flattenHelper :: AnnoyingList a -> AnnoyingList a -> AnnoyingList a
flattenHelper annoy1 newannoy = append annoy1 newannoy

-- (K)  Define a function remove which takes two
--      strings as its arguments and removes every letter from the
--      second list that occurs in the first list. For
--      example, remove "first" "second" = "econd"

remove :: (AnnoyingList Char) -> (AnnoyingList Char) -> (AnnoyingList Char)
remove annoy1 annoy2 = fromList (removeHelper (charList annoy1) (charList annoy2))
charList :: AnnoyingList Char -> [Char]
charList charlist = foldr charHelper [] charlist
charHelper :: Char -> [Char] -> [Char]
charHelper char charlist = (char : charlist)
removeHelper :: [Char] -> [Char] -> [Char]
removeHelper _ [] = []
removeHelper x (y:ys) = if x == []
                        then (y:ys)
                        else if (elem y x)
                             then removeHelper x ys
                             else [y] ++ removeHelper x ys




-- (L) Write average, which takes a list of Floats and returns
--     the average

average :: (AnnoyingList Float) -> Maybe Float
average annoy = if (length annoy) == 0
                then Nothing
                else Just (sum (foldr averageHelper [] annoy) / (fromIntegral (length annoy)))
averageHelper :: Float -> [Float] -> [Float]
averageHelper flo annoy2 = [flo] ++ annoy2


-- Write min_list, which returns the smallest element
-- in a list of positive numbers; it should return 0
-- for the empty list. It should use foldr and the
-- built-in function min.

min_list :: (Ord a,Num a) => (AnnoyingList a) -> a
min_list lst = if (length lst) == 0
               then 0
               else foldr min_listHelper (foldr helper2 0 lst) lst
min_listHelper :: (Ord a,Num a) => a -> a -> a
min_listHelper int smallInt = min int smallInt
helper2 :: (Ord a,Num a) => a -> a -> a
helper2 x y = x


-- Using the following function (what does it do??)
-- write an insertion sort function using only foldr and f

f x xs = [ y | y <- xs, y < x] ++ [x] ++ [z | z <- xs, z >= x]
insertsort :: Ord a => (AnnoyingList a) -> (AnnoyingList a)
insertsort lst  = fromList (foldr f [] lst)
