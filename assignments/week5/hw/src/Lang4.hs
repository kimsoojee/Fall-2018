module Lang4 where
import Data.Map -- for Env

-- We will now add let expressions to the abstract syntax tree.
-- Let String Ast Ast makes a local assignment of the first Ast to the String
-- in the state and evaluates the second Ast in this environment and returns
-- the result of the second Ast.

data Ast =
      AstInt Integer
    | Id String
    | Plus Ast Ast
    | Let String Ast Ast
    | Separator Ast Ast

type Env = Map String Integer

eval :: Env -> Ast -> Maybe Integer
eval e (AstInt x) = Just x
eval e (Id s) = Data.Map.lookup s e
eval e (Plus x y) = case (eval e x, eval e y) of (Just x2, Just y2) -> Just (x2 + y2)
                                                 (Just x2, Nothing) -> Just x2
                                                 (Nothing, Just y2) -> Just y2
                                                 (Nothing, Nothing) -> Nothing
eval e (Let str x y) = case eval e x of (Just a) -> eval (Data.Map.insert str a e) y
                                        (Nothing) -> eval e y
eval e (Separator x y) = eval e y

-- show the unevaluated expression, see tests

instance Show Ast where
  show (AstInt x) = show x
  show (Id s) = s
  show (Plus x y) = show x ++ "+" ++ show y
  show (Let str x y) = "let" ++ str ++ "=" ++ show x ++ "in" ++ show y
  show (Separator x y) = show x ++ ";" ++ show y

-- equality on eval result

instance Eq Ast where
  x == y = eval (Data.Map.empty) x == eval (Data.Map.empty) y
