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
-- A 'Comonad' is the categorical dual of a 'Monad'.
----------------------------------------------------------------------------
module Control.Comonad
  ( 
  -- * Functor and Comonad
    Functor(..)
  , Comonad(..)
  , ComonadZip(..)
  , Cokleisli(..)
  -- * Functions

  -- ** Naming conventions
  -- $naming

  -- ** Operators
  , (=>=)   -- :: Comonad w => (w a -> b) -> (w b -> c) -> w a -> c
  , (=<=)   -- :: Comonad w => (w b -> c) -> (w a -> b) -> w a -> c
  , (=>>)   -- :: Comonad w => w a -> (w a -> b) -> w b
  , (<<=)   -- :: Comonad w => (w a -> b) -> w a -> w b
  , (<..>)  -- :: ComonadZip w => w a -> w (a -> b) -> w b

  -- * Fixed points and folds
  , wfix    -- :: Comonad w => w (w a -> a) -> a
  , unfoldW -- :: Comonad w => (w b -> (a,b)) -> w b -> [a]

  -- ** Comonadic lifting 
  , liftW   -- :: Comonad w => (a -> b) -> w a -> w b
  , liftW2  -- :: ComonadZip w => (a -> b -> c) -> w a -> w b -> w c
  , liftW3  -- :: ComonadZip w => (a -> b -> c -> d) -> w a -> w b -> w c -> w d
  , wzip    -- :: ComonadZip w => w a -> w b -> w (a, b)

  ) where

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Category
import Data.Functor
import Data.Monoid
import Data.Functor.Identity
import Control.Monad.Trans.Identity

infixl 1 =>> 
infixr 1 <<=, =<=, =>= 
infixl 4 <.>, <., .>, <..>

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
-- Promotes a function to a comonad.
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

-- | A generalized comonadic list anamorphism
unfoldW :: Comonad w => (w b -> (a,b)) -> w b -> [a]
unfoldW f w = fst (f w) : unfoldW f (w =>> snd . f)

-- | Comonadic fixed point
wfix :: Comonad w => w (w a -> a) -> a
wfix w = extract w (extend wfix w)

-- * Comonads for Prelude types:

-- Instances: While Control.Comonad.Instances would be more symmetric with the definition of
-- Control.Monad.Instances in base, the reason the latter exists is because of Haskell 98 specifying
-- the types Either a, ((,)m) and ((->)e) and the class Monad without having the foresight to require 
-- or allow instances between them. Here Haskell 98 says nothing about Comonads, so we can include the 
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

-- Provided to avoid an orphan instance. Not proposed to standardize. 
-- If Comonad moved to base, consider moving instance into transformers?
instance Comonad w => Comonad (IdentityT w) where
  extract = extract . runIdentityT
  extend f (IdentityT m) = IdentityT (extend (f . IdentityT) m)

{- | 

As a symmetric semi-monoidal comonad, an instance of ComonadZip is required to satisfy:

> extract (wzip a b) = (extract a, extract b)

By extension, the following law must also hold:

> extract (a <.> b) = extract a (extract b)

Minimum definition: '<.>'

Based on the ComonadZip from "The Essence of Dataflow Programming" 
by Tarmo Uustalu and Varmo Vene, but adapted to fit the conventions of 
Control.Monad and to provide a similar programming style to 
that of Control.Applicative. 

-}
class Comonad w => ComonadZip w where
  -- | 
  -- > (<.>) = liftW2 id
  (<.>) :: w (a -> b) -> w a -> w b

  -- |
  -- > (.>) = liftW2 (const id)
  (.>) :: w a -> w b -> w b
  (.>) = liftW2 (const id)

  -- |
  -- > (<.) = liftW2 const
  (<.) :: w a -> w b -> w a
  (<.) = liftW2 const
  
instance Monoid m => ComonadZip ((,)m) where
  ~(m, a) <.> ~(n, b) = (m `mappend` n, a b)

instance Monoid m => ComonadZip ((->)m) where
  g <.> h = \m -> (g m) (h m)

instance ComonadZip Identity where
  Identity a <.> Identity b = Identity (a b)

instance ComonadZip w => ComonadZip (IdentityT w) where
  IdentityT wa <.> IdentityT wb = IdentityT (wa <.> wb)

(<..>) :: ComonadZip w => w a -> w (a -> b) -> w b
(<..>) = liftW2 (flip ($))
{-# INLINE (<..>) #-}

-- |
-- > wzip wa wb = (,) <$> wa <.> wb
-- > wzip = liftW2 (,) 
-- 
-- Called 'czip' in "Essence of Dataflow Programming"
wzip :: ComonadZip w => w a -> w b -> w (a, b)
wzip = liftW2 (,)
{-# INLINE wzip #-}

liftW2 :: ComonadZip w => (a -> b -> c) -> w a -> w b -> w c
liftW2 f a b = f <$> a <.> b
{-# INLINE liftW2 #-}

liftW3 :: ComonadZip w => (a -> b -> c -> d) -> w a -> w b -> w c -> w d
liftW3 f a b c = f <$> a <.> b <.> c
{-# INLINE liftW3 #-}


-- | The 'Cokleisli' 'Arrow's of a given 'Comonad'
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
    f' wa = f . wzip wa . fmap snd

instance Functor (Cokleisli w a) where
  fmap f (Cokleisli g) = Cokleisli (f . g)

instance Monad (Cokleisli w a) where
  return a = Cokleisli (const a)
  Cokleisli k >>= f = Cokleisli $ \w -> runCokleisli (f (k w)) w

{- $naming

The functions in this library use the following naming conventions, based
on those of Control.Monad.

* A postfix \'@W@\' always stands for a function in the Cokleisli category:
  The monad type constructor @w@ is added to function results
  (modulo currying) and nowhere else.  So, for example, 

>  filter  ::                (a ->   Bool) -> [a] ->   [a]
>  filterW :: (Comonad w) => (w a -> Bool) -> w [a] -> [a]

* A prefix \'@w@\' generalizes an existing function to a comonadic form.
  Thus, for example: 

>  fix  :: (a -> a) -> a
>  wfix :: w (w a -> a) -> a

When ambiguous, consistency with existing Control.Monad combinators supercedes other naming considerations.

-}
