{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Array.Pointed(PArray(..), cursor) where

{- D. Orchard, 2013 
   
   This module defines the standard (immutable) pointed-array comonad, where
   an array is paired with a "cursor" marking the "point" of the array -}

import Data.Array.Base
import Data.Array.IArray
import Control.Comonad

data PArray i e = PArray (Array i e) i deriving (Show, Eq, Ord)

cursor :: PArray i e -> i
cursor (PArray _ c) = c

-- IArray instance is mostly standard for the internal 'Array'.

instance IArray PArray e where
     bounds (PArray x _) = bounds x
     numElements (PArray x _) = numElements x
     unsafeAt (PArray x _) = unsafeAt x
     unsafeArray b es = let a = (unsafeArray b es)
                        in PArray a (fst b) -- set cursor as lower-bound

-- Comonad definition

instance Ix i => Functor (PArray i) where
    fmap = amap 

instance Ix i => Comonad (PArray i) where
    extract (PArray a c) = a!c
    extend k (PArray a c) = let es' = map (\i -> (i, k (PArray a i))) (indices a)
                            in  PArray (array (bounds a) es') c