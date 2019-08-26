module HigherOrderProblems where
import DataProblems


-- HO programming using map and filter

-- I will give them map and filter in lecture on Tuesday

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = (f x) : (map' f xs)

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs) =
   if   (p x)
   then x : (filter' p xs)
   else filter' p xs


returnEven ::  [Integer] -> [Integer]
returnEven x = filter (\a -> a `mod` 2 == 0) x


repeat10Times :: [String] -> [String]
repeat10Times x = map (\a -> a++a++a++a++a++a++a++a++a++a) x

-- take a list of students and return a list of thier names and years

classYear :: Year -> Integer
classYear Freshman = 1
classYear Sophomore = 2
classYear Junior = 3
classYear Senior = 4

students = [(BUstudent "s" "j" "U4" CS Freshman), (BUstudent "J" "K" "U3" CS Sophomore)]

toNameYear :: [Student] -> [(String, Integer)]
toNameYear x = map (\(BUstudent a b c d e) -> (a, classYear e)) x
