module Arith where

import ExceptionMonad
import ParserMonad

-- hint: read about the Rational type, fromIntegral might be helpful

-- write a full arithmetic language, it must have a nice show, eval and parser

-- your language must support numbers , addition, subtraction, multiplication, division

-- your parser must support the above and parentheses, natural numbers, white space, and standard precedence rules

-- your show should look nice, and should be parsable

-- ungraded bonus: handle negative numbers, decimals
-- ungraded bonus: add operations like power or mod

data Arith =
    Num Integer
  | Add Arith Arith
  | Sub Arith Arith
  | Mul Arith Arith
  | Div Arith Arith


eval :: Arith -> Unsafe Rational
eval (Num r) = Ok (fromIntegral r)
eval (Add x y) = do r1 <- eval x
                    r2 <- eval y
                    Ok (r1 + r2)
eval (Sub x y) = do r1 <- eval x
                    r2 <- eval y
                    Ok (r1 - r2)
eval (Mul x y) = do r1 <- eval x
                    r2 <- eval y
                    Ok (r1 * r2)
eval (Div x y) = do r1 <- eval x
                    r2 <- eval y
                    Ok (r1 / r2)


(<||>) :: Parser a -> Parser a -> Parser a
l <||> r = Parser $ \ input ->  case parse l input of
                                                 Just (a, rest) -> Just (a, rest)
                                                 Nothing -> case parse r input of
                                                              Just (b, rest) -> Just (b, rest)
                                                              Nothing        -> Nothing

addSub :: Arith -> Parser Arith
addSub arith = do s <- token $ (literal "+") <|> (literal "-")
                  exp <- multDivEprOrParensOrNat
                  let res = case s of
                              Left _ -> Add arith exp
                              Right _ -> Sub arith exp
                   in (addSub res) <||> return res

addSubExpr :: Parser Arith
addSubExpr = do l <- multDivEprOrParensOrNat
                addSub l

multDiv :: Arith -> Parser Arith
multDiv arith = do s <- token $ ((literal "*") <|>  (literal "/"))
                   exp <- parensOrNat
                   let res = case s of
                                Left _ -> Mul arith exp
                                Right _ -> Div arith exp
                    in (multDiv res) <||> return res


multDivEpr :: Parser Arith
multDivEpr = do l <- parensOrNat
                multDiv l

parens :: Parser Arith
parens = do s <- token $ (literal "(") +++ parser +++ (literal ")")
            return (case s of ((_, a), _) -> a)

nats :: Parser Arith
nats = do s <- token $ natParser
          return (Num s)

parensOrNat :: Parser Arith
parensOrNat = do s <- parens <|> nats
                 return (case s of Left a -> a
                                   Right b -> b)

multDivEprOrParensOrNat :: Parser Arith
multDivEprOrParensOrNat =  do s <- multDivEpr <|> parensOrNat
                              return (case s of Left a -> a
                                                Right b -> b)

parser :: Parser Arith
parser = do s <- addSubExpr <|> multDivEprOrParensOrNat
            return (case s of Left a -> a
                              Right b -> b)



-- a helper function
-- parseArith :: String -> Maybe Arith
-- parseArith s = fmap fst $ parse parser s

-- a human readable string
instance Show Arith where
  show (Num a) = show a
  show (Add x y) = (show x) ++ "+" ++ (show y)
  show (Sub x y) = (show x) ++ "-" ++ (show y)
  show (Mul x y) = (show x) ++ "*" ++ (show y)
  show (Div x y) = (show x) ++ "/" ++ (show y)


-- structural equality on your AST "2 + 2 /= 4" but "2*3 + 4*5" == "(2*3) + (4*5)"
-- mostly for testing
instance Eq Arith where
  x == y = case (x, y) of
              ((Num a), (Num b)) -> a == b
              ((Add a1 a2), (Add b1 b2)) -> (show a1, show a2) == (show b1, show b2)
              ((Sub a1 a2), (Sub b1 b2)) -> (show a1, show a2) == (show b1, show b2)
              ((Mul a1 a2), (Mul b1 b2)) -> (show a1, show a2) == (show b1, show b2)
              ((Div a1 a2), (Div b1 b2)) -> (show a1, show a2) == (show b1, show b2)
              (_, _) -> False
