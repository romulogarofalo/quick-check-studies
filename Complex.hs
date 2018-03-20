module Complex where

import Control.Applicative
import Test.QuickCheck

data Expr = Lit Integer | Add Expr Expr | Mul Expr Expr deriving (Show, Eq)


elimMull :: Expr -> Expr
elimMull (Lit x)         = Lit x
elimMull (Add a b )      = Add (elimMull a) (elimMull b)
elimMull (Mul a (Lit 1)) = elimMull a
elimMull (Mul (Lit 1) a) = elimMull a
elimMull (Mul a b)       = Mul (elimMull a) (elimMull b)

foo = Add(Mul(Lit 1) (Lit 2)) (Add (Add(Lit 1) (Lit 2)) (Lit 4))
bar = Lit 1

prop_elimMull :: Expr -> Bool
prop_elimMull expr = f (elimMull expr) where
    f (Mul _ (Lit 1)) = False
    f (Mul (Lit 1) _) = False
    f (Mul a b) = f a && f b
    f (Add a b) = f a && f b
    f (Lit _) = True


