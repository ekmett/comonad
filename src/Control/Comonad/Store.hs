{-# LANGUAGE Safe #-}

-- |
-- Copyright   :  (C) 2008-2014 Edward Kmett
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
, store
, runStore
-- * The StoreT comonad transformer
, StoreT(..)
, runStoreT
) where

import Control.Comonad.Store.Class (ComonadStore(..))
import Control.Comonad.Trans.Store (Store, store, runStore, StoreT(..), runStoreT)
