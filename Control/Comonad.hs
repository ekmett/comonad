-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad
-- Copyright   :  (C) 2008-2011 Edward Kmett,
--                (C) 2004 Dave Menendez
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Control.Comonad ( 
  -- * Comonads
  -- $definition
    Comonad(..)
  , (=>>)     -- :: Comonad w => w a -> (w a -> b) -> w b
  , (<<=)     -- :: Comonad w => (w a -> b) -> w a -> w b
  , liftW     -- :: Comonad w => (a -> b) -> w a -> w b
  , wfix      -- :: Comonad w => w (w a -> a) -> a

  -- * Cokleisli Arrows
  , Cokleisli(..)
  -- ** Cokleisli composition
  , (=>=)     -- :: Comonad w => (w a -> b) -> (w b -> c) -> w a -> c
  , (=<=)     -- :: Comonad w => (w b -> c) -> (w a -> b) -> w a -> c
  ) where

import Prelude hiding (id, (.))
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad.Trans.Identity
import Data.Functor.Identity
import Data.Monoid
import Data.Typeable

infixl 1 =>> 
infixr 1 <<=, =<=, =>= 

{- | $definition -}

class Functor w => Comonad w where
  -- | 
  -- > extract . fmap f = f . extract
  extract   :: w a -> a
  -- | 
  -- > duplicate = extend id
  -- > fmap (fmap f) . duplicate = duplicate . fmap f
  duplicate :: w a -> w (w a)
  -- |
  -- > extend f  = fmap f . duplicate
  extend    :: (w a -> b) -> w a -> w b

  extend f = fmap f . duplicate
  duplicate = extend id

-- | A suitable default definition for 'fmap' for a 'Comonad'. 
-- Promotes a function to a comonad.
--
-- > fmap f    = extend (f . extract)
liftW :: Comonad w => (a -> b) -> w a -> w b
liftW f = extend (f . extract)
{-# INLINE liftW #-}

-- | 'extend' with the arguments swapped. Dual to '>>=' for a 'Monad'.
(=>>) :: Comonad w => w a -> (w a -> b) -> w b
(=>>) = flip extend
{-# INLINE (=>>) #-}

-- | 'extend' in operator form 
(<<=) :: Comonad w => (w a -> b) -> w a -> w b
(<<=) = extend
{-# INLINE (<<=) #-}

-- | Right-to-left Cokleisli composition 
(=<=) :: Comonad w => (w b -> c) -> (w a -> b) -> w a -> c
f =<= g = f . extend g
{-# INLINE (=<=) #-}

-- | Left-to-right Cokleisli composition
(=>=) :: Comonad w => (w a -> b) -> (w b -> c) -> w a -> c
f =>= g = g . extend f 
{-# INLINE (=>=) #-}

-- | Comonadic fixed point
wfix :: Comonad w => w (w a -> a) -> a
wfix w = extract w (extend wfix w)

-- * Comonads for Prelude types:
--
-- Instances: While Control.Comonad.Instances would be more symmetric
-- to the definition of Control.Monad.Instances in base, the reason
-- the latter exists is because of Haskell 98 specifying the types
-- @'Either' a@, @((,)m)@ and @((->)e)@ and the class Monad without
-- having the foresight to require or allow instances between them.
-- Here Haskell 98 says nothing about Comonads, so we can include the
-- instances directly avoiding the wart of orphan instances.

instance Comonad ((,)e) where
  extract = snd
  duplicate ~(e,a) = (e,(e,a))

instance Monoid m => Comonad ((->)m) where
  extract f = f mempty
  duplicate f m = f . mappend m

-- * Comonads for types from 'transformers'.
--
-- This isn't really a transformer, so i have no compunction about including the instance here.
--
-- TODO: Petition to move Data.Functor.Identity into base
instance Comonad Identity where
  extract = runIdentity
  extend f = Identity . f 
  duplicate = Identity

-- Provided to avoid an orphan instance. Not proposed to standardize. 
-- If Comonad moved to base, consider moving instance into transformers?
instance Comonad w => Comonad (IdentityT w) where
  extract = extract . runIdentityT
  extend f (IdentityT m) = IdentityT (extend (f . IdentityT) m)

-- | The 'Cokleisli' 'Arrow's of a given 'Comonad'
newtype Cokleisli w a b = Cokleisli { runCokleisli :: w a -> b }

instance Typeable1 w => Typeable2 (Cokleisli w) where
  typeOf2 twab = mkTyConApp cokleisliTyCon [typeOf1 (wa twab)]
        where wa :: Cokleisli w a b -> w a
              wa = undefined

cokleisliTyCon :: TyCon
cokleisliTyCon = mkTyCon "Control.Comonad.Cokleisli"
{-# NOINLINE cokleisliTyCon #-}

instance Comonad w => Category (Cokleisli w) where
  id = Cokleisli extract
  Cokleisli f . Cokleisli g = Cokleisli (f =<= g)

instance Comonad w => Arrow (Cokleisli w) where
  arr f = Cokleisli (f . extract)
  first f = f *** id
  second f = id *** f
  Cokleisli f *** Cokleisli g = Cokleisli (f . fmap fst &&& g . fmap snd)
  Cokleisli f &&& Cokleisli g = Cokleisli (f &&& g)

instance Comonad w => ArrowApply (Cokleisli w) where
  app = Cokleisli $ \w -> runCokleisli (fst (extract w)) (snd <$> w)

instance Comonad w => ArrowChoice (Cokleisli w) where
  left = leftApp

-- Cokleisli arrows are actually just a special case of a reader monad:

instance Functor (Cokleisli w a) where
  fmap f (Cokleisli g) = Cokleisli (f . g)

instance Applicative (Cokleisli w a) where
  pure = Cokleisli . const
  Cokleisli f <*> Cokleisli a = Cokleisli (\w -> (f w) (a w))

instance Monad (Cokleisli w a) where
  return = Cokleisli . const
  Cokleisli k >>= f = Cokleisli $ \w -> runCokleisli (f (k w)) w


{- $definition

There are two ways to define a comonad:

I. Provide definitions for 'extract' and 'extend'
satisfying these laws:

> extend extract      = id
> extract . extend f  = f
> extend f . extend g = extend (f . extend g)

In this case, you may simply set 'fmap' = 'liftW'.

These laws are directly analogous to the laws for monads
and perhaps can be made clearer by viewing them as laws stating
that Cokleisli composition must be associative, and has extract for
a unit:

> f =>= extract   = f
> extract =>= f   = f
> (f =>= g) =>= h = f =>= (g =>= h)

II. Alternately, you may choose to provide definitions for 'fmap',
'extract', and 'duplicate' satisfying these laws:

> extract . duplicate      = id
> fmap extract . duplicate = id
> duplicate . duplicate    = fmap duplicate . duplicate

In this case you may not rely on the ability to define 'fmap' in 
terms of 'liftW'.

You may of course, choose to define both 'duplicate' /and/ 'extend'. 
In that case you must also satisfy these laws:

> extend f  = fmap f . duplicate
> duplicate = extend id
> fmap f    = extend (f . extract)

These are the default definitions of 'extend' and'duplicate' and 
the definition of 'liftW' respectively.

-}

