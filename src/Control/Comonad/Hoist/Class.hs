-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Hoist.Class
-- Copyright   :  (C) 2008-2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
----------------------------------------------------------------------------
module Control.Comonad.Hoist.Class (ComonadHoist(..)) where

import Control.Comonad
import Control.Monad.Trans.Identity
import Data.Functor.Identity

class ComonadHoist t where
  -- | Ideally we would offer a way to lift comonad homomorphisms
  -- but this isn't Haskell 98, so we settle for the most common case
  -- here.
  --
  -- > liftTrans :: (forall a. w a -> v a) -> t w a -> t v a 
  -- > cohoist = liftTrans (Identity . extract)
  cohoist :: Comonad w => t w a -> t Identity a

-- avoiding orphans

instance ComonadHoist IdentityT where
  cohoist = IdentityT . Identity . extract . runIdentityT
