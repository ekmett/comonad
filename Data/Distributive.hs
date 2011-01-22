-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Distributive
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Distributive 
  ( Distributive(..)
  , fmapDefault
  ) where

import Control.Applicative
import Control.Comonad
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Instances ()
import Data.Functor.Identity

-- | This is the categorical dual of 'Traversable'
-- 
-- Minimal definition: 'mapW' or 'distribute'
--
-- > mapW = fmap f . duplicate
-- > distribute = mapW id
-- 
-- To be distributable a container will need to have a way to consistently
-- zip a potentially infinite number of copies of itself. This effectively
-- means that the holes in all values of that type, must have the same 
-- cardinality, fixed sized vectors, infinite streams, functions, etc.
-- and no extra information to try to merge together.

class Functor g => Distributive g where
  -- | The dual of 'Data.Traversable.mapM'
  cotraverse :: Comonad w => (w a -> b) -> w (g a) -> g b

  -- | The dual of 'Data.Traversable.sequence'
  distribute :: Comonad w => w (g a) -> g (w a)

  cotraverse f = fmap f . distribute
  distribute = cotraverse id
  
instance Distributive Identity where
  cotraverse f = Identity . f . fmap runIdentity
  distribute = Identity . fmap runIdentity

instance Distributive ((->)e) where
  distribute w e = fmap ($e) w

instance Distributive g => Distributive (ReaderT e g) where
  distribute w = ReaderT $ \e -> distribute (fmap (flip runReaderT e) w)

instance Distributive g => Distributive (IdentityT g) where
  cotraverse f w = IdentityT $ cotraverse f (runIdentityT <$> w)
  distribute w = IdentityT $ distribute (runIdentityT <$> w)

-- | Every 'Distributive' is a 'Functor'. This is a valid default definition.
fmapDefault :: Distributive g => (a -> b) -> g a -> g b
fmapDefault f = cotraverse (f . runIdentity) . Identity
