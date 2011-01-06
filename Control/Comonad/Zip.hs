-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Zip
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- ComonadZip is based on the class from "The Essence of Dataflow Programming" 
-- by Tarmo Uustalu and Varmo Vene. Names have been adapted to fit the
-- conventions of Control.Monad and to provide a similar programming style to 
-- that of Control.Applicative. 
----------------------------------------------------------------------------

module Control.Comonad.Zip
  ( 
  -- * Comonads with zipping
    ComonadZip(..)
  -- * Utility functions
  , (<$>), (<$), (<..>) 
  , liftW, liftW2, liftW3
  , wzip
  ) where

import Control.Comonad
import Data.Functor ((<$>))
import Data.Monoid
import Data.Functor.Identity
import Control.Monad.Trans.Identity

infixl 4 <.>, <., .>, <..>

{- | 

As a symmetric semi-monoidal comonad, an instance of ComonadZip is required to satisfy:

> extract (wzip a b) = (extract a, extract b)

By extension, the following law must also hold:

> extract (a <.> b) = extract a (extract b)

Minimum definition: '<.>'

-}
class Comonad w => ComonadZip w where
  -- | 
  -- > (<.>) = wzipWith id
  (<.>) :: w (a -> b) -> w a -> w b

  -- |
  -- > (.>) = wzipWith (const id)
  (.>) :: w a -> w b -> w b
  (.>) = liftW2 (const id)

  -- |
  -- > (<.) = wzipWith const
  (<.) :: w a -> w b -> w a
  (<.) = liftW2 const
  
instance Monoid m => ComonadZip ((,)m) where
  ~(m, a) <.> ~(n, b) = (m `mappend` n, a b)

instance Monoid m => ComonadZip ((->)m) where
  g <.> h = \m -> (g m) (h m)

instance ComonadZip Identity where
  Identity a <.> Identity b = Identity (a b)

instance ComonadZip w => ComonadZip (IdentityT w) where
  IdentityT wa <.> IdentityT wb = IdentityT (wa <.> wb)

(<..>) :: ComonadZip w => w a -> w (a -> b) -> w b
(<..>) = liftW2 (flip ($))
{-# INLINE (<..>) #-}

-- |
-- > wzip wa wb = (,) <$> wa <.> wb
wzip :: ComonadZip w => w a -> w b -> w (a, b)
wzip wa wb = (,) <$> wa <.> wb
{-# INLINE wzip #-}

liftW2 :: ComonadZip w => (a -> b -> c) -> w a -> w b -> w c
liftW2 f a b = f <$> a <.> b
{-# INLINE liftW2 #-}

liftW3 :: ComonadZip w => (a -> b -> c -> d) -> w a -> w b -> w c -> w d
liftW3 f a b c = f <$> a <.> b <.> c
{-# INLINE liftW3 #-}
