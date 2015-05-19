{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
#if MIN_VERSION_transformers(0,3,0)
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Env.Class
-- Copyright   :  (C) 2008-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (fundeps, MPTCs)
----------------------------------------------------------------------------
module Control.Comonad.Env.Class
  ( ComonadEnv(..)
  , asks
  ) where

import Control.Comonad
import Control.Comonad.Trans.Class
import qualified Control.Comonad.Trans.Env as Env
import Control.Comonad.Trans.Store
import Control.Comonad.Trans.Traced
import Control.Comonad.Trans.Identity
import Data.Semigroup

class Comonad w => ComonadEnv e w | w -> e where
  ask :: w a -> e

asks :: ComonadEnv e w => (e -> e') -> w a -> e'
asks f wa = f (ask wa)
{-# INLINE asks #-}

instance Comonad w => ComonadEnv e (Env.EnvT e w) where
  ask = Env.ask

instance ComonadEnv e ((,)e) where
  ask = fst

#if MIN_VERSION_semigroups(0,16,2)
instance ComonadEnv e (Arg e) where
  ask (Arg e _) = e
#endif

lowerAsk :: (ComonadEnv e w, ComonadTrans t) => t w a -> e
lowerAsk = ask . lower
{-# INLINE lowerAsk #-}

instance ComonadEnv e w => ComonadEnv e (StoreT t w) where
  ask = lowerAsk

instance ComonadEnv e w => ComonadEnv e (IdentityT w) where
  ask = lowerAsk

instance (ComonadEnv e w, Monoid m) => ComonadEnv e (TracedT m w) where
  ask = lowerAsk
