{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad
-- Copyright   :  (C) 2008-2011 Edward Kmett
--      (C) 2004 Dave Menendez
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- A 'Comonad' is the categorical dual of a 'Monad'.
----------------------------------------------------------------------------
module Control.Comonad
  ( Comonad(..)
  , liftW
  , (=>>)
  , (.>>)
  , liftCtx
  , mapW
  , parallelW
  , unfoldW
  , sequenceW
  ) where

import Data.Monoid
import Data.Functor.Identity
import Control.Monad.Trans.Identity

infixl 1 =>>, .>>

{-|
There are two ways to define a comonad:

I. Provide definitions for 'fmap', 'extract', and 'duplicate'
satisfying these laws:

> extract . duplicate      == id
> fmap extract . duplicate == id
> duplicate . duplicate    == fmap duplicate . duplicate

II. Provide definitions for 'extract' and 'extend'
satisfying these laws:

> extend extract      == id
> extract . extend f  == f
> extend f . extend g == extend (f . extend g)

('fmap' cannot be defaulted, but a comonad which defines
'extend' may simply set 'fmap' equal to 'liftW'.)

A comonad providing definitions for 'extend' /and/ 'duplicate',
must also satisfy these laws:

> extend f  == fmap f . duplicate
> duplicate == extend id
> fmap f    == extend (f . extract)

(The first two are the defaults for 'extend' and 'duplicate',
and the third is the definition of 'liftW'.)
-}

class Functor w => Comonad w where
  extract:: w a -> a
  duplicate :: w a -> w (w a)
  extend :: (w a -> b) -> w a -> w b

  extend f = fmap f . duplicate
  duplicate = extend id

-- | A suitable default definition for 'fmap' for a 'Comonad'.
liftW :: Comonad w => (a -> b) -> w a -> w b
liftW f = extend (f . extract)

-- | 'extend' with the arguments swapped. Dual to '>>=' for a 'Monad'.
(=>>) :: Comonad w => w a -> (w a -> b) -> w b
(=>>) = flip extend

-- | Injects a value into the comonad.
(.>>) :: Comonad w => w a -> b -> w b
w .>> b = extend (\_ -> b) w

-- | Transform a function into a comonadic action
liftCtx :: Comonad w => (a -> b) -> w a -> b
liftCtx f = extract . fmap f

mapW :: Comonad w => (w a -> b) -> w [a] -> [b]
mapW f w
  | null (extract w) = []
  | otherwise        = f (fmap head w) : mapW f (fmap tail w)

parallelW :: Comonad w => w [a] -> [w a]
parallelW w
  | null (extract w) = []
  | otherwise        = fmap head w : parallelW (fmap tail w)

unfoldW :: Comonad w => (w b -> (a,b)) -> w b -> [a]
unfoldW f w = fst (f w) : unfoldW f (w =>> snd . f)

-- | Converts a list of comonadic functions into a single function
-- returning a list of values
sequenceW :: Comonad w => [w a -> b] -> w a -> [b]
sequenceW []     _ = []
sequenceW (f:fs) w = f w : sequenceW fs w

-- * Comonads for Prelude types:

-- Instances: While Control.Comonad.Instances would be more symmetric with the definition of
-- Control.Monad.Instances in base, the reason the latter exists is because of Haskell 98 specifying
-- the types Either a, ((,)m) and ((->)e) AND the class Monad without having the foresight to require 
-- or allow the instances. Here Haskell 98 says nothing about Comonads, so we can include the 
-- instances directly avoiding the wart of orphan instances.

instance Comonad ((,)e) where
  extract = snd
  duplicate ~(e,a) = (e,(e,a))

instance Monoid m => Comonad ((->)m) where
  extract f = f mempty
  duplicate f m = f . mappend m

-- * Comonads for types from 'transformers'.

-- This isn't really a transformer, so i have no compunction about including the instance here.
-- TODO: Petition to move Data.Functor.Identity into base
instance Comonad Identity where
  extract = runIdentity
  extend f = Identity . f 
  duplicate = Identity

-- Provided to avoid an orphan instance. Not proposed to standardize
instance Comonad w => Comonad (IdentityT w) where
  extract = extract . runIdentityT
  extend f (IdentityT m) = IdentityT (extend (f . IdentityT) m)
