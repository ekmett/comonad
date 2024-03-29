{-# LANGUAGE Safe #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Copyright   :  (C) 2008-2014 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (fundeps, MPTCs)

module Control.Comonad.Traced
(
-- * ComonadTraced class
  ComonadTraced(..)
, traces
-- * The Traced comonad
, Traced
, pattern Traced
, runTraced
-- * The TracedT comonad transformer
, TracedT(..)
) where

import Control.Comonad.Traced.Class (ComonadTraced(..), traces)
import Control.Comonad.Trans.Traced (Traced, pattern Traced, runTraced, TracedT(..), runTracedT)
