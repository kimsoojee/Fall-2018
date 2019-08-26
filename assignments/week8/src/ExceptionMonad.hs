module ExceptionMonad where
import Control.Monad(ap)

data Unsafe a = Error String | Ok a deriving (Show, Eq)

instance Functor Unsafe where
  -- fmap :: (a -> b) -> Unsafe a -> Unsafe b
  fmap f (Error s) =  Error s
  fmap f (Ok a) =  Ok (f a)
  -- hint: f :: a -> b


--ignore this for now
instance Applicative Unsafe where
  pure = return
  (<*>) = ap

instance Monad Unsafe where
  --return :: a -> Unsafe a
  return a = Ok a

  --(>>=) :: Unsafe a -> (a -> Unsafe b) -> Unsafe b
  (Error s) >>= f  =  Error s
  (Ok a) >>= f  = f a
  -- hint: f :: a -> Unsafe b
