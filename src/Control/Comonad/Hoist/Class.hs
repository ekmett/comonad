{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}

-- |
-- Copyright   :  (C) 2008-2021 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable

module Control.Comonad.Hoist.Class
( ComonadHoist(cohoist)
) where

import Control.Comonad
import Control.Monad.Trans.Identity

class ComonadHoist t where
  -- | Given any comonad-homomorphism from @w@ to @v@ this yields a comonad
  -- homomorphism from @t w@ to @t v@.
  cohoist :: (Comonad w, Comonad v) => (forall x. w x -> v x) -> t w a -> t v a

instance ComonadHoist IdentityT where
  cohoist l = IdentityT . l . runIdentityT
  {-# inline cohoist #-}
