module Arbitro where

import Control.Applicative
import Test.QuickCheck

arbitraryList :: (Arbitrary a, Show a) => Gen [a]
arbitraryList =
  sized $
    \n -> do
      k <- choose (0, n)
      sequence [ arbitrary | _ <- [1..k] ]



data Tree a = Tree a [Tree a] deriving (Show)

aTree :: Tree Int
aTree = Tree 5 [Tree 12 [Tree (-16) []],Tree 10 [],Tree 16 [Tree 12 []]]

aTree' :: Tree Int
aTree' = Tree 5 [Tree 12 [Tree (-16) []],Tree 10 [],Tree 16 [Tree 12 []], Tree 2 [Tree 4 []]] -- mudar a arvore para teste

nodes :: Tree a -> Int
nodes (Tree y []) = 1
nodes (Tree y lista) = 1 + (sum $ map nodes lista)

edges :: Tree a -> Int
edges (Tree y []) = 0
edges (Tree y lista) = 0 + (sum $ map edges lista) + (length lista)


prop_OneMoreNodeThanEdges :: Tree Int -> Bool
prop_OneMoreNodeThanEdges tree = nodes tree == edges tree + 1
  

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary =
      sized arbitrarySizedTree
  
arbitrarySizedTree :: Arbitrary a => Int -> Gen (Tree a)
arbitrarySizedTree m = do
    t <- arbitrary
    n <- choose (0, m `div` 2)
    ts <- vectorOf n (arbitrarySizedTree (m `div` 4))
    return (Tree t ts)


prop_MaxLe :: Int -> Int -> Property
prop_MaxLe x y = x <= y ==> max x y == y


main :: IO()
main = quickCheck (prop_OneMoreNodeThanEdges)

