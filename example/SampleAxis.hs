#!/usr/bin/env runhaskell
{-# OPTIONS -Wall #-}
import Data.Tensor.TypeLevel
import Data.Tensor.TypeLevel.Axis as Ax

v :: Vec4 Double
v = Vec :~ 0 :~ 1 :~ 2 :~ 3

a :: Axis Vec4
a = Axis 0


main :: IO ()
main = do
  print v
  print a
  print $ v!a
  print $ Ax.dimension a
  print $ Ax.next a
  print $ Ax.prev a
  print $ Ax.others $ Ax.next a
  print $ Ax.all a
  print $ Ax.allFrom $ Ax.next a
