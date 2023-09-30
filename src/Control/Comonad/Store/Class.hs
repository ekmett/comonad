{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright   :  (C) 2008-2021 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (fundeps, MPTCs)

module Control.Comonad.Store.Class
( ComonadStore(..)
, lowerPos
, lowerPeek
) where

import Control.Comonad
import Control.Comonad.Trans.Class
import Control.Comonad.Trans.Env
import qualified Control.Comonad.Trans.Store as Store
import Control.Comonad.Trans.Traced
import Control.Comonad.Trans.Identity

class Comonad w => ComonadStore s w | w -> s where
  pos :: w a -> s

  peek :: s -> w a -> a
  peek = peeks . const

  peeks :: (s -> s) -> w a -> a
  peeks f w = peek (f (pos w)) w
  {-# inline peeks #-}

  seek :: s -> w a -> w a
  seek s = peek s . duplicate
  {-# inline seek #-}

  seeks :: (s -> s) -> w a -> w a
  seeks f = peeks f . duplicate
  {-# inline seeks #-}

  experiment :: Functor f => (s -> f s) -> w a -> f a
  experiment f w = fmap (`peek` w) (f (pos w))
  {-# inline experiment #-}

#if __GLASGOW_HASKELL__ >= 708
  {-# MINIMAL pos, (peek | peeks) #-}
#endif

instance Comonad w => ComonadStore s (Store.StoreT s w) where
  pos = Store.pos
  peek = Store.peek
  peeks = Store.peeks
  seek = Store.seek
  seeks = Store.seeks
  experiment = Store.experiment
  {-# inline pos #-}
  {-# inline peek #-}
  {-# inline peeks #-}
  {-# inline seek #-}
  {-# inline seeks #-}
  {-# inline experiment #-}

lowerPos :: (ComonadTrans t, ComonadStore s w) => t w a -> s
lowerPos = pos . lower
{-# inline lowerPos #-}

lowerPeek :: (ComonadTrans t, ComonadStore s w) => s -> t w a -> a
lowerPeek s = peek s . lower
{-# inline lowerPeek #-}

lowerExperiment :: (ComonadTrans t, ComonadStore s w, Functor f) => (s -> f s) -> t w a -> f a
lowerExperiment f = experiment f . lower
{-# inline lowerExperiment #-}

instance ComonadStore s w => ComonadStore s (IdentityT w) where
  pos = lowerPos
  peek = lowerPeek
  experiment = lowerExperiment
  {-# inline pos #-}
  {-# inline peek #-}
  {-# inline experiment #-}

instance ComonadStore s w => ComonadStore s (EnvT e w) where
  pos = lowerPos
  peek = lowerPeek
  experiment = lowerExperiment
  {-# inline pos #-}
  {-# inline peek #-}
  {-# inline experiment #-}

instance (ComonadStore s w, Monoid m) => ComonadStore s (TracedT m w) where
  pos = lowerPos
  peek = lowerPeek
  experiment = lowerExperiment
  {-# inline pos #-}
  {-# inline peek #-}
  {-# inline experiment #-}
