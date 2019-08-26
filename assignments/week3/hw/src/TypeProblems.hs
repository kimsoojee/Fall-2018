module TypeProblems where


data Answer a = Impossible | Example a deriving Show


-- Give Example definitions that have the following types if it is possible, If not return "Impossible".
--It does not matter what the definitions actually do as long AS they are correctly typed, they do not "loop forever", and they do not use undefined.

q1 :: Answer [Bool]
q1 = Example exq1
exq1 :: [Bool]
exq1 = [True, True, False]

q2 :: Answer ([(Int,Char)],Bool)
q2 = Example ([(3,'a')], True)


q3 :: Answer (Int -> Bool -> Char -> Int -> Bool)
q3 = Example exq3
exq3 :: Int -> Bool -> Char -> Int -> Bool
exq3 i b c i2 = b

q4 :: Answer (a -> ([a],[a],[a]))
q4 = Example exq4
exq4 :: a -> ([a],[a],[a])
exq4 x = ([x],[x],[x])

q5 :: Answer (a -> b)
q5 = Impossible


q6 :: Answer (a -> a)
q6 = Example exq6
exq6 :: a -> a
exq6 x = x

-- but different then above
q7 :: Answer (a -> a)
q7 = Impossible

q8 :: Answer (a -> a -> a)
q8 = Example exq8
exq8 :: a -> a -> a
exq8 _ x = x

-- but different then above
q9 :: Answer (a -> a -> a)
q9 = Example exq9
exq9 :: a -> a -> a
exq9 x _ = x



q10 :: Answer (a -> b -> a)
q10 = Example exq10
exq10 :: a -> b -> a
exq10 x y = x

q11 :: Answer (a -> b -> b)
q11 = Example exq11
exq11 :: a -> b -> b
exq11 x y = y

q12 :: Answer ((a -> b) -> b)
q12 = Impossible

q13 :: Answer ((a -> b) -> (Maybe b))
q13 = Example exq13
exq13 :: (a -> b) -> (Maybe b)
exq13 x = Nothing


q14 :: Answer (a -> b -> (b,a))
q14 = Example exq14
exq14 :: a -> b -> (b,a)
exq14 x y = (y,x)


q15 :: Answer ((a -> b) -> (b -> c) -> (a -> c))
q15 = Example anything'
anything' :: (a -> b) -> (b -> c) -> a -> c
anything' a2b b2c justA = (b2c (a2b justA))


q16 :: Answer ((a -> b) -> (b -> c) -> (c -> a))
q16 = Impossible
anything :: (a -> b) -> (b -> c) -> c -> a
anything a2b b2c justC = undefined


q17 :: Answer ((a -> b) -> ([b] -> c) -> c)
q17 = Impossible
