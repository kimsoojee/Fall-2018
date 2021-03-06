module InfiniteLists where


{-  Problem One: Infinite lists and lazy evaluation.
    This question consists of a "prequestion" with multiple parts
    asking you to think about, predict, and then verify the
    result of various operations on infinite lists (no credit, but
    skip it at your peril!).

    The coding part of the question has multiple parts where you
    will define various infinite lists or functions that use
    infinite lists in some essential way.

-}


{-
Prequestion: Can you predict what will happen with the following
expressions? Will it disappear forever (the technical term
is that it "diverges") or give an answer? Or both (meaning, give
a part of an answer and then diverge). Think about it,
make a prediction, and then verify with ghci.

NOTE: You can cancel output in ghci by typing control-c. In the worst
case if you are in a Unix system you can suspend the whole process
 using control-z and then kill it:

Prelude> nats = map (+1) nats
Prelude> nats
^Z
[1]+  Stopped                 ghci
HW11 $ kill %1

[1]+  Stopped                 ghci



Prelude> x = [1..]

--(i)

Prelude> head x

--(ii)

Prelude> tail x

--(iii)

Prelude> head (tail (tail x))

--(iv)

Prelude> zip x (tail x)

--(v)

Prelude> take 5 x

--(vi)

Prelude> elem 100000 x

--(vii)

Prelude> elem (-3) x

--(viii)

Prelude> filter (<5) x

--(ix)

Prelude> takeWhile (<5) x

--(x)

Prelude> zzz = 0 : zzz

Prelude> take 5 zzz

--(xi)

Prelude> rep k = k : (rep k)

Prelude> take 5 (rep k)

--(xii)

Prelude> take 5 (zip x (tail x))

--(xiii)

Prelude> take 10 (x ++ x)

--(xiv)

Prelude> foldr (+) 0 x

--(xv)

Prelude> t = [x*2 | x <- t]

Prelude> take 10 t

--(xvi)

Prelude> t = 1:[x*2 | x <- t]

Prelude> take 10 t

--(xvii)

Prelude> take 5 ("0":["S("++x++")" | x <- t])

--(xviii)

Prelude> f n = 1:[x*n| x <- (f n)]

Prelude> take 5 (f 5)

--(xix1

Prelude> nats = map (+1) nats

Prelude> take 10 nats

--(xx)

Prelude> nats = 1:(map (+1) nats)

Prelude> take 10 nats


-}


-- Ok the rest are to be completed in Haskell and handed in!

-- Ungraded bonus: write these functions in one line, without
-- helper functions or multiple cases or if-then-else. Use
-- the higher-order list functions!

-- HINT: look at the tests to see examples!


--(A) Write an expression (not a function, but a variable such
--    as in (ix) above) which represents the infinite list
--         [0,1,0,-1,0,1,0,-1,0,.....]

aList :: [Integer]
aList = [0,1,0,-1] ++ aList



--(B) Write a function repeatList which take a finite list of Integers
--    and repeats the list infinitely--see the tests for its behavior.

repeatList :: [Integer] -> [Integer]
repeatList lst = lst ++ (repeatList lst)


--(C) Write a function interleave which takes two lists
--    and alternates them (alternately taking elements from each list).
--    If one list terminates the result should terminate
--    interleave [9,8] [1..] = [9,1,8,2]
--    If both lists go forever the resulting list should go forever

interleave :: [a] -> [a] -> [a]
interleave [] _ = []
interleave _ [] = []
interleave (x:xs) (y:ys) = x : y : (interleave xs ys)


--(D) Write a function expand which takes a list (possibly) infinite
--    of numbers, and repeats each number k in the list k times. See
--    the tests for examples. Hint: find out what the Prelude functions
--    zipWith and replicate do.

expand :: [Int] -> [Int]
expand [] = []
expand (x:xs) = (replicate x x) ++ (expand xs)


--(E) Write a definition of the infinite list of factorials = [1!, 2!, ....]
-- Hint: use the functions map and product OR the function scanl.

factorials :: [Integer]
factorials = scanl (\x y -> x*y) 1 [2..]


--(F) Write a function (iter f x) which creates an infinite list
--    [x, (f x), (f (f x)), .....].

iter :: (a -> a) -> a -> [a]
iter f x = x : (iter f (f x))


--(G) Next, write the function (collatz1 k), which generates the next
-- integer in the Collatz Sequence starting at n:
--
--   collatz1 1 = 1; if n is even, collatz1 n = n/2, otherwise,  = 3*n+1
--

collatz1 :: Integer -> Integer
collatz1 int
      | int == 1 = 1
      | even int = div int 2
      | otherwise = 3 * int + 1

-- Then write the function (collatz n), which generates the
-- collatz sequence, in which the collatz1 function is applied
-- to n until the result is 1 (it is conjectured, but not proved,
-- that there are no infinite such sequences). For example,
-- starting with n = 12, one gets the sequence 12, 6, 3, 10, 5, 16, 8, 4, 2, 1
-- and for n = 19 we have 19, 58, 29, 88, 44, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1.
-- For n = 27, it takes 111 steps, getting as large as 9232 before descending and finally reaching 1.
-- Hint: generate an infinite sequence, filter it using takeWhile,
-- and then append a 1 to the end.

collatz :: Integer -> [Integer]
collatz n = takeWhile (>1) (n : collatz (collatz1 n)) ++ [1]


--(H) Write a function (linCong a b c x) which takes a value x and
-- returns (a*x+b) `mod` c and then define a function
-- (randSeq seed a b c) which defines an infinite (but repeating)
-- sequence of "random" numbers which starts with seed and applies
-- the linCong function to get the next number in the sequence.

linCong :: Integer -> Integer -> Integer -> Integer -> Integer
linCong a b c x = mod (a * x + b) c


randomSequence :: Integer -> Integer -> Integer -> Integer -> [Integer]
randomSequence seed a b c = seed : (randomSequence (linCong a b c seed) a b c)


-- (I) Write a function fibonnacci which creates the infinite list of
-- Fibonacci numbers. Hint: in the recursion, you need to combine the sequences
-- fibonacci and (tail fibonacci) in some way. Could use zip or zipWith.

fibonacci :: [Integer]
fibonacci = (scanl (+) 1 (0 : fibonacci))


-- (J) Write a definition of the sequence of Hamming numbers,
--     which is all numbers which have the factors 2, 3, and 5.
--     Hint: use the helper function shown.

-- Return a list of all factors of a number n (hint: use a filter
-- on the list [1..n]

factors :: Integer -> [Integer]
factors n = [x | x <- [1..n], n `mod`x == 0]


hamming :: [Integer]
hamming = [x | x <- [1..], elem 2 (factors x) && elem 3 (factors x) && elem 5 (factors x) ]


-- (K) Write a definition of primes == infinite list of all prime numbers,
-- using the helper functions shown. Use the helper function from the
-- previous problem.


isPrime :: Integer -> Bool
isPrime n = length [ x | x <- [2..n], n `mod` x == 0] == 1

-- Now define the infinite list of primes

primes :: [Integer]
primes = [ x | x <- [1..], isPrime x]


--(L) Pascal's Triangle: write a function (pascalRow k) which
--    returns the (infinite) kth row of the triangle as follows:
--   0: 1  1  1  1  1  1 ...
--   1: 1  2  3  4  5  6 ...
--   2: 1  3  6  10 15 21 ...
--   3: 1  4  10 20 35 56 ...
-- etc.

pascalRow :: Integer -> [Integer]
pascalRow n = (zipWith (\x y -> choose x y) [n,n+1..] [0..])

-- From Pascal's Triangle we can easily define "N choose K":
--   N choose K = 0 if K > N,
--              = N choose (N-K) if K > N/2
--              = (N-K)th element of row K of Pascal's Triangle otherwise.
-- This uses the previous function, but does not otherwise involve infinite lists;
-- just an example application of the previous.

choose :: Integer -> Integer ->Integer
-- row n, term k
choose n k
       | n < k = 0
       | k == 0 || (n - k) == 0 = 1
       | (div n 2) < k = choose n (n - k)
       | otherwise = div (factorials !! fromInteger(n - 1)) ((factorials !! fromInteger(k - 1)) * (factorials !! fromInteger(n - k - 1)))
