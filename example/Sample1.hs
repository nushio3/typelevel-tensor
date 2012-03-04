#!/usr/bin/env runhaskell
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
import qualified Algebra.Ring as Ring
import Data.Tensor.TypeLevel
import NumericPrelude

infixl 7 .* -- scalar product
(.*) :: (Vector v, Ring.C a) => a -> v a -> v a
x .* ys = compose $ \i -> x * ys!i

infix 7 `dot` -- inner product
dot :: (Vector v, Ring.C a) => v a -> v a -> a
x `dot` y = contract $ \i -> x!i * y!i

infix 7 `cross` -- cross product
cross :: (Ring.C a) => Vec3 a -> Vec3 a -> Vec3 a
x `cross` y = compose $ \i -> 
  let 
    Axis n = i
    j = Axis (mod (n+1) 3)
    k = Axis (mod (n+2) 3)
    in x!j * y!k - x!k * y!j

v, w:: Vec3 Double
v = Vec :~ 2 :~ 3 :~ 4 
w = Vec :~ 3 :~ 4 :~ 5 

main :: IO ()
main = do
  print $ 10 .* v
  print $ v + w  
  print $ v `dot` w  
  print $ v `cross` w
