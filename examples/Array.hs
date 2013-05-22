{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Comonad

import Data.Array.IArray
import Data.Array.Pointed

x :: PArray (Int, Int) Float
x = array ((0, 0), (2, 2)) [((0,0),1.0), ((0,1), 2.0), ((0,2), 3.0),
                              ((1,0),4.0), ((1,1), 5.0), ((1,2), 6.0),
                              ((2,0),7.0), ((2,1), 8.0), ((2,2), 9.0)]

x' = extend laplace2D x

laplace2D :: (Fractional a) => PArray (Int, Int) a -> a
laplace2D a = a ? (-1, 0) + a ? (1, 0) + a ? (0, -1) + a ? (0, 1) - 4 * a ? (0, 0)

(?) :: (Ix i, Fractional a, Num i) => PArray i a -> i -> a
a ? i' = let i = cursor a
         in if (inRange (bounds a) (i+i')) then a!(i+i') else 0.0

instance (Num a, Num b) => Num (a, b) where
    (x, y) + (a, b) = (x + a, y  + b)
    (x, y) - (a, b) = (x - a, y - b)
    (x, y) * (a, b) = (x * a, y * b)
    abs (x, y) = (abs x, abs y)
    signum (x, y) = (signum x, signum y)
    fromInteger x = (fromInteger x, fromInteger x)


