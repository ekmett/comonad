{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Safe #-}

-- |
-- Copyright   :  (C) 2008-2021 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- The environment comonad holds a value along with some retrievable context.
--
-- This module specifies the environment comonad transformer (aka coreader),
-- which is left adjoint to the reader comonad.
--
-- The following sets up an experiment that retains its initial value in the
-- background:
--
-- >>> let initial = env 0 0
--
-- Extract simply retrieves the value:
--
-- >>> extract initial
-- 0
--
-- Play around with the value, in our case producing a negative value:
--
-- >>> let experiment = fmap (+ 10) initial
-- >>> extract experiment
-- 10
--
-- Oh noes, something went wrong, 10 isn't very negative! Better restore the
-- initial value using the default:
--
-- >>> let initialRestored = experiment =>> ask
-- >>> extract initialRestored
-- 0

module Control.Comonad.Trans.Env
(
-- * The strict environment comonad
  Env
, pattern Env
, runEnv
-- * The strict environment comonad transformer
, EnvT(..)
, runEnvT
, lowerEnvT
-- * Combinators
, ask
, asks
, local
) where

import Control.Comonad
import Control.Comonad.Hoist.Class
import Control.Comonad.Trans.Class
import Data.Data
import Data.Functor.Identity
import GHC.Generics

-- $setup
-- >>> import Control.Comonad

type Env e = EnvT e Identity
data EnvT e w a = EnvT e (w a)
  deriving (Generic, Generic1, Data)

-- | Create an Env using an environment and a value
pattern Env :: e -> a -> Env e a
pattern Env e a = EnvT e (Identity a)

runEnv :: Env e a -> (e, a)
runEnv (EnvT e (Identity a)) = (e, a)

runEnvT :: EnvT e w a -> (e, w a)
runEnvT (EnvT e wa) = (e, wa)

instance Functor w => Functor (EnvT e w) where
  fmap g (EnvT e wa) = EnvT e (fmap g wa)

instance Comonad w => Comonad (EnvT e w) where
  duplicate (EnvT e wa) = EnvT e (extend (EnvT e) wa)
  extract (EnvT _ wa) = extract wa

instance ComonadTrans (EnvT e) where
  lower (EnvT _ wa) = wa

instance (Monoid e, Applicative m) => Applicative (EnvT e m) where
  pure = EnvT mempty . pure
  EnvT ef wf <*> EnvT ea wa = EnvT (ef `mappend` ea) (wf <*> wa)

-- | Gets rid of the environment. This differs from 'extract' in that it will
--   not continue extracting the value from the contained comonad.
lowerEnvT :: EnvT e w a -> w a
lowerEnvT (EnvT _ wa) = wa

instance ComonadHoist (EnvT e) where
  cohoist l (EnvT e wa) = EnvT e (l wa)

instance (Semigroup e, ComonadApply w) => ComonadApply (EnvT e w) where
  EnvT ef wf <@> EnvT ea wa = EnvT (ef <> ea) (wf <@> wa)

instance Foldable w => Foldable (EnvT e w) where
  foldMap f (EnvT _ w) = foldMap f w

instance Traversable w => Traversable (EnvT e w) where
  traverse f (EnvT e w) = EnvT e <$> traverse f w

-- | Retrieves the environment.
ask :: EnvT e w a -> e
ask (EnvT e _) = e

-- | Like 'ask', but modifies the resulting value with a function.
--
--   > asks = f . ask
asks :: (e -> f) -> EnvT e w a -> f
asks f (EnvT e _) = f e

-- | Modifies the environment using the specified function.
local :: (e -> e') -> EnvT e w a -> EnvT e' w a
local f (EnvT e wa) = EnvT (f e) wa
