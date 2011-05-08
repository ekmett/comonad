-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Extend
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Functor.Extend
  ( -- * $definition
    Extend(..)
  , (=>>)     -- :: Extend w => w a -> (w a -> b) -> w b
  , (<<=)     -- :: Extend w => (w a -> b) -> w a -> w b
  , (=>=)     -- :: Extend w => (w a -> b) -> (w b -> c) -> w a -> c
  , (=<=)     -- :: Extend w => (w b -> c) -> (w a -> b) -> w a -> c
  ) where

import Prelude hiding (id, (.))
import Control.Category
import Control.Monad.Trans.Identity
import Data.Functor.Identity
import Data.Semigroup
import Data.List (tails)
import Data.List.NonEmpty (NonEmpty(..), toList)
import Data.Sequence (Seq) 
import qualified Data.Sequence as Seq
import Data.Tree

infixl 1 =>> 
infixr 1 <<=, =<=, =>= 

class Functor w => Extend w where
  -- | 
  -- > duplicate = extend id
  -- > fmap (fmap f) . duplicate = duplicate . fmap f
  duplicate :: w a -> w (w a)
  -- |
  -- > extend f  = fmap f . duplicate
  extend    :: (w a -> b) -> w a -> w b

  extend f = fmap f . duplicate
  duplicate = extend id

-- | 'extend' with the arguments swapped. Dual to '>>=' for a 'Monad'.
(=>>) :: Extend w => w a -> (w a -> b) -> w b
(=>>) = flip extend
{-# INLINE (=>>) #-}

-- | 'extend' in operator form 
(<<=) :: Extend w => (w a -> b) -> w a -> w b
(<<=) = extend
{-# INLINE (<<=) #-}

-- | Right-to-left Cokleisli composition 
(=<=) :: Extend w => (w b -> c) -> (w a -> b) -> w a -> c
f =<= g = f . extend g
{-# INLINE (=<=) #-}

-- | Left-to-right Cokleisli composition
(=>=) :: Extend w => (w a -> b) -> (w b -> c) -> w a -> c
f =>= g = g . extend f 
{-# INLINE (=>=) #-}

-- * Extends for Prelude types:
--
-- Instances: While Data.Functor.Extend.Instances would be symmetric
-- to the definition of Control.Monad.Instances in base, the reason
-- the latter exists is because of Haskell 98 specifying the types
-- @'Either' a@, @((,)m)@ and @((->)e)@ and the class Monad without
-- having the foresight to require or allow instances between them.
--
-- Here Haskell 98 says nothing about Extend, so we can include the
-- instances directly avoiding the wart of orphan instances.

instance Extend [] where
  duplicate = tails

instance Extend Maybe where
  duplicate Nothing = Nothing
  duplicate j = Just j

instance Extend (Either a) where
  duplicate (Left a) = Left a
  duplicate r = Right r

instance Extend ((,)e) where
  duplicate p = (fst p, p)

instance Semigroup m => Extend ((->)m) where
  duplicate f m = f . (<>) m

instance Extend Seq where
  duplicate = Seq.tails

instance Extend Tree where
  duplicate w@(Node _ as) = Node w (map duplicate as)

-- I can't fix the world
-- instance (Monoid m, Extend n) => Extend (ReaderT m n) 
--   duplicate f m = f . mappend m

-- * Extends for types from 'transformers'.
--
-- This isn't really a transformer, so i have no compunction about including the instance here.
--
-- TODO: Petition to move Data.Functor.Identity into base
instance Extend Identity where
  duplicate = Identity

-- Provided to avoid an orphan instance. Not proposed to standardize. 
-- If Extend moved to base, consider moving instance into transformers?
instance Extend w => Extend (IdentityT w) where
  extend f (IdentityT m) = IdentityT (extend (f . IdentityT) m)

instance Extend NonEmpty where
  extend f w@ ~(_ :| aas) = f w :| case aas of
      []     -> []
      (a:as) -> toList (extend f (a :| as))

{- $definition

There are two ways to define an 'Extend' instance:

I. Provide definitions for 'extend'
satisfying this law:

> extend f . extend g = extend (f . extend g)

II. Alternately, you may choose to provide definitions for 'duplicate' 
satisfying this laws:

> duplicate . duplicate    = fmap duplicate . duplicate

These are both equivalent to the statement that (=>=) is associative

> (f =>= g) =>= h = f =>= (g =>= h)

You may of course, choose to define both 'duplicate' /and/ 'extend'. 
In that case you must also satisfy these laws:

> extend f  = fmap f . duplicate
> duplicate = extend id

These are the default definitions of 'extend' and 'duplicate'.

-}
