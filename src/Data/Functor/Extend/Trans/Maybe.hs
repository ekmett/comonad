-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Extend.Trans.Maybe
-- Copyright   :  (C) 2008-2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
----------------------------------------------------------------------------
module Data.Functor.Extend.Trans.Maybe
  ( MaybeT(..), maybeT, nothing, justT ) where

import Control.Applicative
import Data.Functor.Apply
import Data.Functor.Alt
import Data.Functor.Extend

newtype MaybeT w a = MaybeT { runMaybeT :: Maybe (w a) }

instance Functor w => Functor (MaybeT w) where
  fmap f = MaybeT . fmap (fmap f) . runMaybeT

instance Extend w => Extend (MaybeT w) where
  duplicated (MaybeT (Just w)) = MaybeT $ Just $ extended (MaybeT . Just) w
  duplicated (MaybeT Nothing) = MaybeT Nothing

maybeT :: a -> (w b -> a) -> MaybeT w b -> a
maybeT z f = maybe z f . runMaybeT

nothing :: MaybeT w a
nothing = MaybeT Nothing

justT :: w a -> MaybeT w a
justT = MaybeT . Just

instance Apply w => Apply (MaybeT w) where
  MaybeT a <.> MaybeT b = MaybeT $ (<.>) <$> a <.> b

instance Applicative w => Applicative (MaybeT w) where
  pure = MaybeT . Just . pure
  MaybeT a <*> MaybeT b = MaybeT $ (<*>) <$> a <*> b

-- TODO: weaken Alt
instance Apply w => Alt (MaybeT w) where
  MaybeT a <!> MaybeT b = MaybeT $ a <!> b

instance Applicative w => Alternative (MaybeT w) where
  empty = MaybeT Nothing
  MaybeT a <|> MaybeT b = MaybeT $ a <|> b
