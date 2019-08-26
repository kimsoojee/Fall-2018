module MonadProblems where

-- type classes have some rules, Haskell people call these rules "Laws"

-- monads have the rules
-- left identity,  return a >>= k  =  k a
-- right identity,  m >>= return    =  m
-- associativity,  m >>= (\x -> k x >>= h)  =  (m >>= k) >>= h
-- (copied from https://wiki.haskell.org/Typeclassopedia#Monad)

-- let's break some laws


-- let's break the left identity

data NotLI a = NotLeft (Maybe a) deriving (Show, Eq) -- replace Undefined with your constructors
-- note Eq must be defined correctly over NotLI, "deriving Eq" will do this for you


-- you may ignore these 2 instances, just here to make Haskell happy
instance Functor NotLI where
  fmap f ma =  ma >>= (\x -> return $ f x)

instance Applicative NotLI where
  pure  = return
  mf <*> y = mf >>= (\f -> fmap f y)

instance Monad NotLI where
  --return :: a -> NotLI a
  return a = NotLeft (Just a)

  --(>>=) :: NotLI a -> (a -> NotLI b) -> NotLI b
  (NotLeft a) >>= f  = case (NotLeft a) of
                          NotLeft _ -> NotLeft Nothing



-- give an examples that show the identity law being broken  "(return aNum >>= aFunc) /=  (aFunc aNum)"

aNum :: Integer
aNum = 100

aFunc :: Integer -> NotLI String
aFunc = \x -> NotLeft (Just (show x))


-- let's break the associativity law
data NotAs a = NotAssoc (Either (Maybe a) (Maybe a)) deriving (Show, Eq) -- replace Undefined2 with your constructors
-- note Eq must be defined correctly over NotLI, "deriving Eq" will do this for you


-- you may ignore these 2 instances, just here to make Haskell happy
instance Functor NotAs where
  fmap f ma =  ma >>= (\x -> return $ f x)

instance Applicative NotAs where
  pure  = return
  mf <*> y = mf >>= (\f -> fmap f y)

instance Monad NotAs where
  --return :: a -> NotAs a
  return a = NotAssoc (Right (Just a))

  --(>>=) :: NotAs a -> (a -> NotAs b) -> NotAs b
  (NotAssoc a) >>= f  = case NotAssoc a of
                           NotAssoc (Left (Just a)) -> NotAssoc (Right (Nothing))
                           NotAssoc (Right (Nothing)) -> NotAssoc (Left (Nothing))
                           NotAssoc (Left (Nothing)) -> NotAssoc (Right (Nothing))
                           NotAssoc (Right (Just a)) -> NotAssoc (Left (Nothing))


-- give an examples that show the identity law being broken  "(mNum >>= (\x -> kFun x >>= hFun)) /=  ((mNum >>= kFun) >>= hFun)"


mNum :: NotAs Integer
mNum = NotAssoc (Right (Just 200))

kFun :: Integer -> NotAs String
kFun = \x -> NotAssoc (Left (Just (show x)))

hFun :: String -> NotAs Bool
hFun = \y -> NotAssoc (Right (Just True))

-- (mNum >>= (\x -> kFun x >>= hFun)) /=  ((mNum >>= kFun) >>= hFun)


-- Hints: don't read if you want to spoil the fun




-- NotLI: you can make a bad maybe
-- NotAs can be done with 2 constructors and a "case _ of" in the definition of >>=
-- the functions mostly don't matter
