module WarmUp where

-- create a list that is never empty

data NonEmptyList a = Cons a (NonEmptyList a) | Or a -- deriving Show

example :: NonEmptyList Bool
example = Cons False (Or True)

len :: NonEmptyList a -> Integer
len (Or _) = 1
len(Cons _ x) = 1 + len x


-- we'll come back to this later


instance  Eq a => Eq (NonEmptyList a) where
  (Or x) == (Or y) = x == y
  (Cons x xs) == (Cons y ys) = x == y && xs == ys
  _ == _ = False


  --instance  Show a => Show (NonEmptyList a) where
  --  show x = undefined

instance Show a => Show (NonEmptyList a) where
  show (Or x) = "[" ++ show x ++ "]"
  show (Cons x xs) = "[" ++ commasBetween xs ++ "]"


commasBetween :: Show a => NonEmptyList a -> String
commasBetween (Or x) = show x
commasBetween (Cons x xs) = show x ++ "," ++ commasBetween xs


-- Think about what rules should go along with each class

--insult :: a -> String
-- insult _ = "You are all Bad"

class Insultable a where
  insult :: a -> String
  cheerUp :: a -> String
  cheerUp x =  (insult x) ++ " you're ok, that's not really true,"

-- insult some types

instance  Insultable Bool where
  insult False = "False you stink"
  insult True = "You are ok"

instance  Insultable Integer where
  insult x = "..."
