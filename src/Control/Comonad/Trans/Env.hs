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
-- >>> let initial = Env 0 0
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
#if MIN_VERSION_base(4,9,0)
import Data.Functor.Classes
#endif
import Data.Functor.Identity
import GHC.Generics

-- $setup
-- >>> import Control.Comonad

type Env e = EnvT e Identity
data EnvT e w a = EnvT e (w a)
  deriving (Eq, Ord, Show, Generic, Generic1, Data)

-- | Create an Env using an environment and a value
pattern Env :: e -> a -> Env e a
pattern Env e a = EnvT e (Identity a)

runEnv :: Env e a -> (e, a)
runEnv = \(EnvT e (Identity a)) -> (e, a)
{-# inline runEnv #-}

runEnvT :: EnvT e w a -> (e, w a)
runEnvT = \(EnvT e wa) -> (e, wa)
{-# inline runEnvT #-}

#if MIN_VERSION_base(4,9,0)
instance (Eq e, Eq1 w) => Eq1 (EnvT e w) where
  liftEq eq (EnvT e1 wa1) (EnvT e2 wa2) = e1 == e2 && liftEq eq wa1 wa2

instance (Ord e, Ord1 w) => Ord1 (EnvT e w) where
  liftCompare comp (EnvT e1 wa1) (EnvT e2 wa2) = case compare e1 e2 of
    EQ -> liftCompare comp wa1 wa2
    r -> r

instance (Show e, Show1 w) => Show1 (EnvT e w) where
  liftShowsPrec sp sl p (EnvT e wa) =
    showParen (p > 10)
    $ showString "EnvT"
    . foldMap (showString " " .) [showsPrec 11 e, liftShowsPrec sp sl 11 wa]
#endif

instance Functor w => Functor (EnvT e w) where
  fmap = \g (EnvT e wa) -> EnvT e (fmap g wa)
  {-# inline fmap #-}

instance Comonad w => Comonad (EnvT e w) where
  duplicate = \(EnvT e wa) -> EnvT e (extend (EnvT e) wa)
  extract = \(EnvT _ wa) -> extract wa
  {-# inline duplicate #-}
  {-# inline extract #-}

instance ComonadTrans (EnvT e) where
  lower = \(EnvT _ wa) -> wa
  {-# inline lower #-}

instance (Monoid e, Applicative m) => Applicative (EnvT e m) where
  pure = EnvT mempty . pure
  {-# inline pure #-}
  (<*>) = \(EnvT ef wf) (EnvT ea wa) -> EnvT (ef `mappend` ea) (wf <*> wa)
  {-# inline (<*>) #-}

-- | Gets rid of the environment. This differs from 'extract' in that it will
--   not continue extracting the value from the contained comonad.
lowerEnvT :: EnvT e w a -> w a
lowerEnvT = \(EnvT _ wa) -> wa
{-# inline lowerEnvT #-}

instance ComonadHoist (EnvT e) where
  cohoist = \l (EnvT e wa) -> EnvT e (l wa)
  {-# inline cohoist #-}

instance (Semigroup e, ComonadApply w) => ComonadApply (EnvT e w) where
  (<@>) = \(EnvT ef wf) (EnvT ea wa) -> EnvT (ef <> ea) (wf <@> wa)
  {-# inline (<@>) #-}

instance Foldable w => Foldable (EnvT e w) where
  foldMap = \f (EnvT _ w) -> foldMap f w
  {-# inline foldMap #-}

instance Traversable w => Traversable (EnvT e w) where
  traverse = \f (EnvT e w) -> EnvT e <$> traverse f w
  {-# inline traverse #-}

-- | Retrieves the environment.
ask :: EnvT e w a -> e
ask = \(EnvT e _) -> e
{-# inline ask #-}

-- | Like 'ask', but modifies the resulting value with a function.
--
--   > asks = f . ask
asks :: (e -> f) -> EnvT e w a -> f
asks = \f (EnvT e _) -> f e
{-# inline asks #-}

-- | Modifies the environment using the specified function.
local :: (e -> e') -> EnvT e w a -> EnvT e' w a
local = \f (EnvT e wa) -> EnvT (f e) wa
{-# inline local #-}
