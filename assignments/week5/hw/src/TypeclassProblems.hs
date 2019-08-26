module TypeclassProblems where

-- define data type for all 7 days of the week
data DayOfTheWeek = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday deriving Show
-- often built-in haskell classes come with nice syntax, when the following is defined try this
-- in the console [Monday .. Friday]

instance Enum DayOfTheWeek where
  --toEnum :: Integer -> DayOfTheWeek (start counting at 0)
  toEnum 0 = Sunday
  toEnum 1 = Monday
  toEnum 2 = Tuesday
  toEnum 3 = Wednesday
  toEnum 4 = Thursday
  toEnum 5 = Friday
  toEnum 6 = Saturday

  --fromEnum :: DayOfTheWeek -> Integer
  fromEnum Sunday = 0
  fromEnum Monday = 1
  fromEnum Tuesday = 2
  fromEnum Wednesday = 3
  fromEnum Thursday = 4
  fromEnum Friday = 5
  fromEnum Saturday = 6

-- First we will work with a custom type class that makes an example of a type

class HasExample a where
  example :: a

-- finish up the instances on these types
instance HasExample DayOfTheWeek where
  example = Sunday

instance HasExample Bool where
  example = True

instance HasExample Integer where
  example = 5

instance HasExample a => HasExample [a] where
  example = [example]

instance (HasExample a, HasExample b) => HasExample (a,b) where
  example = (example, example)

instance HasExample b => HasExample (a -> b) where
  example = (\a -> example)

iSureWishIHadThisFunction :: Integer -> Bool -> (a -> b) -> (Integer, (Bool, DayOfTheWeek))
iSureWishIHadThisFunction = example -- it's a little silly but the code is automatically generated!

-- next we will work with a custom type class that gives all the things

class AllTheThings a where
  listOfAll :: [a]
-- laws: finite, no repeat

-- when we have this defined we can check that ALL inputs of a function are correct
forAll :: AllTheThings a => (a -> Bool) -> Bool
forAll f = all f listOfAll

instance AllTheThings Bool where
  listOfAll = [True, False]

boolEq  :: Bool -> Bool
boolEq = (\b  -> b == b)

--try "forAll boolEq" in the console!

-- finish up the instances on these types
instance AllTheThings DayOfTheWeek where
  listOfAll = [Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday]


instance (AllTheThings a, AllTheThings b) => AllTheThings (a,b) where
  listOfAll = [(a,b) | a <- listOfAll, b <- listOfAll]

-- Bonus challenge problems! (you will not be graded on these)

instance (AllTheThings a, Eq a, AllTheThings b) => AllTheThings (a -> b) where
  listOfAll = undefined

instance (AllTheThings a, Show a, Show b) => Show (a -> b) where
  show f = undefined
