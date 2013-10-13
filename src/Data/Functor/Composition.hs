module Data.Functor.Composition
  ( Composition(..) ) where

import Data.Functor.Compose

-- | We often need to distinguish between various forms of Functor-like composition in Haskell in order to please the type system.
-- This lets us work with these representations uniformly.
class Composition o where
  decompose :: o f g x -> f (g x)
  compose :: f (g x) -> o f g x

instance Composition Compose where
  decompose = getCompose
  compose = Compose
