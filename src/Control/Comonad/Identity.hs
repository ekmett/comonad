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
-- Module      :  Control.Comonad.Identity
-- Copyright   :  (C) 2008-2014 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (fundeps, MPTCs)
----------------------------------------------------------------------------
module Control.Comonad.Identity (
    module Control.Comonad
  , module Data.Functor.Identity
  , module Control.Comonad.Trans.Identity
  ) where

import Control.Comonad
import Data.Functor.Identity
import Control.Comonad.Trans.Identity
