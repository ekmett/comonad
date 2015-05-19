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
-- Module      :  Control.Comonad.Trans.Class
-- Copyright   :  (C) 2008-2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
----------------------------------------------------------------------------
module Control.Comonad.Trans.Class
  ( ComonadTrans(..) ) where

import Control.Comonad
import Control.Monad.Trans.Identity

class ComonadTrans t where
  lower :: Comonad w => t w a -> w a

-- avoiding orphans
instance ComonadTrans IdentityT where
  lower = runIdentityT
