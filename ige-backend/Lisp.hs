module Lisp (Expr(..), eval) where
import Data.Text

data Expr = Add Expr Expr | Atom Int

eval :: Expr -> Int
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Atom a) = a
