-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Arrow.Cokleisli
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Control.Arrow.Cokleisli
  ( Cokleisli
  ) where

import Prelude hiding (id, (.))
import Control.Category
import Control.Comonad
import Control.Comonad.Zip
import Control.Arrow

newtype Cokleisli w a b = Cokleisli { runCokleisli :: w a -> b }

instance Comonad w => Arrow (Cokleisli w) where
  arr f = Cokleisli (f . extract)
  first f = f *** id
  second f = id *** f
  Cokleisli f *** Cokleisli g = Cokleisli (f . fmap fst &&& g . fmap snd)
  Cokleisli f &&& Cokleisli g = Cokleisli (f &&& g)

instance Comonad w => Category (Cokleisli w) where
  id = Cokleisli extract
  Cokleisli f . Cokleisli g = Cokleisli (f =<= g)

instance Comonad w => ArrowApply (Cokleisli w) where
  app = Cokleisli $ \w -> runCokleisli (fst (extract w)) (snd <$> w)

instance Comonad w => ArrowChoice (Cokleisli w) where
  left = leftApp

instance ComonadZip d => ArrowLoop (Cokleisli d) where
  loop (Cokleisli f) = Cokleisli (fst . wfix . extend f') where 
    f' da = f . wzip da . fmap snd

instance Functor (Cokleisli w a) where
  fmap f (Cokleisli g) = Cokleisli (f . g)

instance Monad (Cokleisli d a) where
  return a = Cokleisli (const a)
  Cokleisli k >>= f = Cokleisli $ \w -> runCokleisli (f (k w)) w

