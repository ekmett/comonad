{-# LANGUAGE Safe #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Copyright   :  (C) 2008-2021 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (fundeps, MPTCs)

module Control.Comonad.Store 
(
-- * ComonadStore class
  ComonadStore(..)
-- * The Store comonad
, Store
, pattern Store
, runStore
-- * The StoreT comonad transformer
, StoreT(..)
, runStoreT
) where

import Control.Comonad.Store.Class (ComonadStore(..))
import Control.Comonad.Trans.Store (Store, pattern Store, runStore, StoreT(..), runStoreT)
