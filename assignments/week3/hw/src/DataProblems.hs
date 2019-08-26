module DataProblems where

-- Define a datatype for deserts: deserts consist of cake, cookies, and icecream.
-- Cookies have a number (how many?),
-- Icecream can be flavored like anything (a string),
-- and cake must be flavored like a desert.

data Deserts = Cake String
             | Cookies Integer
             | Icecream String deriving Show

favorite :: Deserts
favorite = Cake "Strawberry"


dontLike :: Deserts
dontLike = Cookies 100


willILikeIt :: Deserts -> Bool
willILikeIt (Cake x) = if x == "Strawberry"
                       then True
                       else False
willILikeIt (Cookies x) = if x == 100
                          then False
                          else True
willILikeIt (Icecream x) = if x == "Strawberry"
                           then True
                           else False


-- Define a datatype for a student at BU.
-- Students should have a first name, a last name, a BU ID, a major, and a class year (Freshman, etc.).
-- Names and the BU ID should be Strings, and the others should be represented by other datatypes
-- (you can pick five majors as examples). </p>



data Student = BUstudent String String String Major Year deriving Show
data Major = CS | IR | CE | Biology | Psychology deriving Show
data Year = Freshman | Sophomore | Junior | Senior deriving Show




me :: Student
me = BUstudent "SooJee" "Kim" "U45689710" CS Senior


-- Make a student from a first name, a last name, a BU ID, and a class year.
-- This functions should just return "CS" majors.

mkStudent :: String -> String -> String -> Integer -> Maybe Student

mkStudent f l i y = if y == 1
                    then Just (BUstudent f l i CS Freshman)
                    else if y == 2
                         then Just (BUstudent f l i CS Sophomore)
                         else if y == 3
                              then Just (BUstudent f l i CS Junior)
                              else if y == 4
                                   then Just (BUstudent f l i CS Senior)
                                   else Nothing
