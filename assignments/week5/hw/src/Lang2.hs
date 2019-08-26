module Lang2 where

-- We will now add a print command to the abstract syntax tree. print takes one expression,
-- evaluates it, prints the result, and finally evaluates to that result.

data Ast =
    AstInt Integer
  | Plus Ast Ast
  | Separator Ast Ast
  | Print Ast


eval :: Ast -> (Integer, [Integer])
eval (AstInt x) = (x,[])
eval (Plus x y) = case (eval x, eval y) of ((x2, printx), (y2, printy)) -> (x2 + y2, printx ++ printy)
eval (Separator x y) = case (eval y) of (y2, printy) -> (y2, printy)
eval (Print x) = case (eval x) of (x2, printx) -> (x2, printx ++ [x2])


-- show the unevaluated expression, see tests

instance Show Ast where
  show (AstInt x) = show x
  show (Plus x y) = (show x) ++ "+" ++ (show y)
  show (Separator x y) = (show x) ++ ";" ++ (show y)
  show (Print x) = "print" ++ show x




-- equality on evaluated result, including [Int]

instance Eq Ast where
  x == y = eval x == eval y
