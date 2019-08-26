module Lang3 where
import Data.Map -- for state, similar to index tree

-- We will now add identifiers and a global assignment command to the abstract syntax tree.
-- Assignment should evaluate to the value of the assignment and store the value in the global memory state.
-- The state (containing values for variables) is passed along as the evaluation proceeds; as Assign
-- expressions are evaluated, bindings are added to the state, and when Id expressions are evaluated
-- they are looked up in the state. Imagine walking around the AST in preorder and keeping track
-- of the state as we do so.

data Ast =
      AstInt Integer
    | Id String
    | Plus Ast Ast
    | Assign String Ast
    | Separator Ast Ast


type State = Map String Integer

-- hint use Data.Map.lookup

eval :: State -> Ast -> (State, Maybe Integer)
eval s (AstInt x) = (s, Just x)
eval s (Id str) = (s, Data.Map.lookup str s)
eval s (Plus x y) = case ((eval s x), (eval s y)) of ((s2, Just x2), (s3, Just y2)) -> (Data.Map.union s2 s3, Just (x2 + y2))
                                                     ((s2, Just x2), (s3, Nothing)) -> (Data.Map.union s2 s3, Just x2)
                                                     ((s2, Nothing), (s3, Just y2)) -> (Data.Map.union s2 s3, Just y2)
                                                     ((s2, Nothing), (s3, Nothing)) -> (Data.Map.union s2 s3, Nothing)
eval s (Assign str a) = case eval s a of (s2, Just x) -> (Data.Map.insert str x s2, Just x)
                                         (s2, Nothing) -> (s2, Nothing)
eval s (Separator x y) = eval s y



-- show the pre evaluated expression, see tests

instance Show Ast where
  show (AstInt x) = show x
  show (Id str) = str
  show (Plus x y) = show x ++ "+" ++ show y
  show (Assign str x) = str ++ "=" ++ show x
  show (Separator x y) = show x ++ ";" ++ show y

-- equality on eval result, including state

instance Eq Ast where
   x == y = eval (Data.Map.empty) x == eval (Data.Map.empty) y
