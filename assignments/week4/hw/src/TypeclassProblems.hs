module TypeclassProblems where

-- type classes have some rules, Haskell people call these rules "Laws"

-- for instance the following should be true for any Eq instance
-- Reflexive: for all x.  x == x
-- Symmetric: for all x, y. x == y iff y == x
-- Transitive: for all x, y, z. if x == y and y == z then x == z

-- let's break some laws

-- let's break the Reflexive law
data NotReflexive = NotReflexiveVal Integer

instance Eq NotReflexive where
  (NotReflexiveVal x) == (NotReflexiveVal y) = if x == y
                                               then False
                                               else True


-- give an example where "example /= example"
example :: NotReflexive
example = NotReflexiveVal 1

-- let's break the Symmetric law
data NotSymmetric = NotSymmetricVal Integer

instance Eq NotSymmetric where
  (NotSymmetricVal x) == (NotSymmetricVal y) = if x < y
                                               then True
                                               else False
-- give an examples where "examplex' == exampley'" but "exampley' /= examplex'"
examplex' :: NotSymmetric
examplex' = NotSymmetricVal 1

exampley' :: NotSymmetric
exampley' = NotSymmetricVal 2

-- let's break the Transitive law
data NotTransitive  = NotTransitiveVal Integer

instance Eq NotTransitive where
  (NotTransitiveVal x) == (NotTransitiveVal y) = if abs(x-y) == 1
                                                 then True
                                                 else False

-- give an examples where "examplex'' == exampley''" and exampley'' == examplez''"  but "examplex'' /= examplez''"
examplex'' :: NotTransitive
examplex'' = NotTransitiveVal 1

exampley'' :: NotTransitive
exampley'' = NotTransitiveVal 2

examplez'' :: NotTransitive
examplez'' = NotTransitiveVal 3


-- there is a more interesting type class called a functor that defined how something is "mappable". It has the laws
-- fmap respects identity: fmap id = id
-- fmap respects composition: fmap (g . f) = fmap g . fmap f


data ListNoIdentity a = NillNi | ConsNi a (ListNoIdentity a) deriving (Show, Eq)

instance Functor ListNoIdentity where
  fmap f NillNi = NillNi
  fmap f (ConsNi a ls) = ConsNi (f a) (fmap f NillNi)

-- give an example list where "fmap id exampleBadId /= exampleBadId"
exampleBadId :: ListNoIdentity Integer
exampleBadId = ConsNi 3 (ConsNi 5 NillNi)

-- here we will break the composition law really bad
data ListNoComposition a = NillNc | ConsNc a (ListNoComposition a) deriving (Show, Eq)

instance Functor ListNoComposition where
  fmap f NillNc = NillNc
  fmap f (ConsNc a ls) = ConsNc (f a) (fmap f NillNc)

-- give example functions and list so that, "fmap (g . f) ls /= (fmap g . fmap f) ls "

ls :: ListNoComposition Integer
ls = ConsNc 2 (ConsNc 4 NillNc)

f :: Integer -> Integer
f int = int + 1

g :: Integer -> Integer
g int = int * 5
