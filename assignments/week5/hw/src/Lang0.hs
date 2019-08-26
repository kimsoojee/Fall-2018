module Lang0 where

-- Consider the language defined by the following AST for adding numbers

data Ast =
    AstInt Integer
  | Plus Ast Ast

-- it can be evaluated with the function:

eval :: Ast -> Integer
eval (AstInt x) = x
eval (Plus x y) = (eval x) + (eval y)

-- show the unevaluated expression, see tests

instance Show Ast where
  show (AstInt x) = show x
  show (Plus x y) = (show x) ++ "+" ++ (show y)

-- equality on the values returned after evaluation

instance Eq Ast where
  x == y = eval x == eval y

-- order on eval result

instance Ord Ast where
  x <= y = eval x <= eval y
