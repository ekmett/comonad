{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE PatternSynonyms #-}


-- |
-- Copyright   :  (C) 2008-2021 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (fundeps, MPTCs)
--
-- The Env comonad (aka the Coreader, Environment, or Product comonad)
--
-- A co-Kleisli arrow in the Env comonad is isomorphic to a Kleisli arrow
-- in the reader monad.
--
-- @
-- (a -> e -> m) ~ (a, e) -> m ~ 'Env' e a -> m
-- @
module Control.Comonad.Env
(
-- * ComonadEnv class
  ComonadEnv(..)
, asks
, local
-- * The Env comonad
, Env
, pattern Env
, runEnv
-- * The EnvT comonad transformer
, EnvT(..)
, runEnvT
) where

import Control.Comonad.Env.Class (ComonadEnv(..), asks)
import Control.Comonad.Trans.Env (Env, pattern Env, runEnv, EnvT(..), runEnvT, local)
