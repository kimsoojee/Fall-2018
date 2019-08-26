module WarmUp where

data Mark = Myself Mark

data Crazier = Wallburgers Mark Integer String Float
               | Trash Crazier Crazier Crazier Bool Char [Bool]
               | Randle (Crazier, Bool)
               | Haskell ([Integer] -> Integer)
               -- deriving Show

-- make a Crazier
example :: Crazier
example = Haskell sum


-- use Crazier
isItNice :: Crazier -> Bool
isItNice (Haskell x)  = (x [1,2,3]) == 6
isItNice _ = False
