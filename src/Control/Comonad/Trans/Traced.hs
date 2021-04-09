{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Safe #-}
#ifdef MIN_VERSION_indexed_traversable
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
#endif

-- |
-- Copyright   :  (C) 2008-2021 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- The trace comonad builds up a result by prepending monoidal values to each
-- other.
--
-- This module specifies the traced comonad transformer (aka the cowriter or
-- exponential comonad transformer).

module Control.Comonad.Trans.Traced
(
-- * Traced comonad
  Traced
, pattern Traced
, runTraced
-- * Traced comonad transformer
, TracedT(..)
-- * Operations
, trace
, listen
, listens
, censor
) where

import Control.Monad (ap)
import Control.Comonad
import Control.Comonad.Hoist.Class
import Control.Comonad.Trans.Class
import Data.Functor.Identity

#ifdef MIN_VERSION_indexed_traversable
import Data.Functor.WithIndex
#endif

import GHC.Generics

type Traced m = TracedT m Identity

pattern Traced :: (m -> a) -> Traced m a
pattern Traced { runTraced } = TracedT (Identity runTraced)

newtype TracedT m w a = TracedT { runTracedT :: w (m -> a) }
  deriving (Generic, Generic1)

instance Functor w => Functor (TracedT m w) where
  fmap g = TracedT . fmap (g .) . runTracedT
  {-# inline fmap #-}

instance (ComonadApply w, Monoid m) => ComonadApply (TracedT m w) where
  TracedT wf <@> TracedT wa = TracedT (ap <$> wf <@> wa)
  {-# inline (<@>) #-}

instance Applicative w => Applicative (TracedT m w) where
  pure = TracedT . pure . const
  {-# inline pure #-}
  TracedT wf <*> TracedT wa = TracedT (ap <$> wf <*> wa)
  {-# inline (<*>) #-}

instance (Comonad w, Monoid m) => Comonad (TracedT m w) where
  extend f = TracedT . extend (\wf m -> f (TracedT (fmap (. mappend m) wf))) . runTracedT
  {-# inline extend #-}
  extract (TracedT wf) = extract wf mempty
  {-# inline extract #-}

instance Monoid m => ComonadTrans (TracedT m) where
  lower = fmap ($ mempty) . runTracedT
  {-# inline lower #-}

instance ComonadHoist (TracedT m) where
  cohoist l = TracedT . l . runTracedT
  {-# inline cohoist #-}

#ifdef MIN_VERSION_indexed_traversable
instance FunctorWithIndex i w => FunctorWithIndex (s, i) (TracedT s w) where
  imap f (TracedT w) = TracedT $ imap (\k' g k -> f (k, k') (g k)) w
  {-# INLINE imap #-}
#endif

trace :: Comonad w => m -> TracedT m w a -> a
trace m (TracedT wf) = extract wf m
{-# inline trace #-}

listen :: Functor w => TracedT m w a -> TracedT m w (a, m)
listen = TracedT . fmap (\f m -> (f m, m)) . runTracedT
{-# inline listen #-}

listens :: Functor w => (m -> b) -> TracedT m w a -> TracedT m w (a, b)
listens g = TracedT . fmap (\f m -> (f m, g m)) . runTracedT
{-# inline listens #-}

censor :: Functor w => (m -> m) -> TracedT m w a -> TracedT m w a
censor g = TracedT . fmap (. g) . runTracedT
{-# inline censor #-}
