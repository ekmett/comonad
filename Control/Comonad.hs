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
module Control.Comonad ( 
  -- * Functors
    Functor(..)
  , (<$>)     -- :: Functor f => (a -> b) -> f a -> f b
  , ( $>)     -- :: Functor f => f a -> b -> f b 

  -- * Comonads
  , Comonad(..)
  , (=>=)     -- :: Comonad w => (w a -> b) -> (w b -> c) -> w a -> c
  , (=<=)     -- :: Comonad w => (w b -> c) -> (w a -> b) -> w a -> c
  , (=>>)     -- :: Comonad w => w a -> (w a -> b) -> w b
  , (<<=)     -- :: Comonad w => (w a -> b) -> w a -> w b
  , liftW     -- :: Comonad w => (a -> b) -> w a -> w b
  , wfix      -- :: Comonad w => w (w a -> a) -> a

  -- * FunctorApply - strong lax symmetric semimonoidal endofunctors
  , FunctorApply(..)
  , (<..>)    -- :: FunctorApply w => w a -> w (a -> b) -> w b
  , liftF2    -- :: FunctorApply w => (a -> b -> c) -> w a -> w b -> w c
  , liftF3    -- :: FunctorApply w => (a -> b -> c -> d) -> w a -> w b -> w c -> w d

  -- * ComonadApply - strong lax symmetric semimonoidal comonads
  , ComonadApply
  , liftW2    -- :: ComonadApply w => (a -> b -> c) -> w a -> w b -> w c
  , liftW3    -- :: ComonadApply w => (a -> b -> c -> d) -> w a -> w b -> w c -> w d

  -- * Wrappers
  , Cokleisli(..)
  , WrappedApplicative(..)
  , WrappedApply(..)
  ) where

import Prelude hiding (id, (.))
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad.Trans.Identity
import Data.Functor
import Data.Functor.Identity
import Data.Monoid

infixl 1 =>> 
infixr 1 <<=, =<=, =>= 
infixl 4 <.>, <., .>, <..>, $>

($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

{- |

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

-- | A strong lax symmetric semi-monoidal functor.

class Functor f => FunctorApply f where
  (<.>) :: f (a -> b) -> f a -> f b

  -- | a .> b = const id <$> a <.> b
  (.>) :: f a -> f b -> f b
  a .> b = const id <$> a <.> b

  -- | a <. b = const <$> a <.> b
  (<.) :: f a -> f b -> f a
  a <. b = const    <$> a <.> b

-- this only requires a Semigroup
instance Monoid m => FunctorApply ((,)m) where
  (<.>) = (<*>)
  (<. ) = (<* )
  ( .>) = ( *>)

-- this only requires a Semigroup
instance Monoid m => FunctorApply ((->)m) where
  (<.>) = (<*>)
  (<. ) = (<* )
  ( .>) = ( *>)

instance FunctorApply ZipList where
  (<.>) = (<*>)
  (<. ) = (<* )
  ( .>) = ( *>)

instance FunctorApply [] where
  (<.>) = (<*>)
  (<. ) = (<* )
  ( .>) = ( *>)

instance FunctorApply IO where
  (<.>) = (<*>)
  (<. ) = (<* )
  ( .>) = ( *>)

instance FunctorApply Maybe where
  (<.>) = (<*>)
  (<. ) = (<* )
  ( .>) = ( *>)

instance FunctorApply Identity where
  (<.>) = (<*>)
  (<. ) = (<* )
  ( .>) = ( *>)

instance FunctorApply w => FunctorApply (IdentityT w) where
  IdentityT wa <.> IdentityT wb = IdentityT (wa <.> wb)

instance Monad m => FunctorApply (WrappedMonad m) where
  (<.>) = (<*>) 
  (<. ) = (<* )
  ( .>) = ( *>)

instance Monoid m => FunctorApply (Const m) where
  (<.>) = (<*>) 
  (<. ) = (<* )
  ( .>) = ( *>)

instance Arrow a => FunctorApply (WrappedArrow a b) where
  (<.>) = (<*>) 
  (<. ) = (<* )
  ( .>) = ( *>)

-- | Wrap an 'Applicative' to be used as a member of 'FunctorApply'
newtype WrappedApplicative f a = WrappedApplicative { unwrapApplicative :: f a } 

instance Functor f => Functor (WrappedApplicative f) where
  fmap f (WrappedApplicative a) = WrappedApplicative (f <$> a)

instance Applicative f => FunctorApply (WrappedApplicative f) where
  WrappedApplicative f <.> WrappedApplicative a = WrappedApplicative (f <*> a)
  WrappedApplicative a <.  WrappedApplicative b = WrappedApplicative (a <*  b)
  WrappedApplicative a  .> WrappedApplicative b = WrappedApplicative (a  *> b)

instance Applicative f => Applicative (WrappedApplicative f) where
  pure = WrappedApplicative . pure
  WrappedApplicative f <*> WrappedApplicative a = WrappedApplicative (f <*> a)
  WrappedApplicative a <*  WrappedApplicative b = WrappedApplicative (a <*  b)
  WrappedApplicative a  *> WrappedApplicative b = WrappedApplicative (a  *> b)
  
-- | Transform a FunctorApply into an Applicative by adding a unit.
newtype WrappedApply f a = WrapApply { unwrapApply :: Either (f a) a }

instance Functor f => Functor (WrappedApply f) where
  fmap f (WrapApply (Right a)) = WrapApply (Right (f     a ))
  fmap f (WrapApply (Left fa)) = WrapApply (Left  (f <$> fa))

instance FunctorApply f => FunctorApply (WrappedApply f) where
  WrapApply (Right f) <.> WrapApply (Right a) = WrapApply (Right (f        a ))
  WrapApply (Right f) <.> WrapApply (Left fa) = WrapApply (Left  (f    <$> fa))
  WrapApply (Left ff) <.> WrapApply (Right a) = WrapApply (Left  (($a) <$> ff))
  WrapApply (Left ff) <.> WrapApply (Left fa) = WrapApply (Left  (ff   <.> fa))

  WrapApply a         <. WrapApply (Right _) = WrapApply a
  WrapApply (Right a) <. WrapApply (Left fb) = WrapApply (Left (a  <$ fb))
  WrapApply (Left fa) <. WrapApply (Left fb) = WrapApply (Left (fa <. fb))

  WrapApply (Right _) .> WrapApply b = WrapApply b
  WrapApply (Left fa) .> WrapApply (Right b) = WrapApply (Left (fa $> b ))
  WrapApply (Left fa) .> WrapApply (Left fb) = WrapApply (Left (fa .> fb))
  
instance FunctorApply f => Applicative (WrappedApply f) where
  pure a = WrapApply (Right a)
  (<*>) = (<.>)
  (<* ) = (<. )
  ( *>) = ( .>)

instance Comonad f => Comonad (WrappedApply f) where
  extract (WrapApply (Right a)) = a
  extract (WrapApply (Left fa)) = extract fa
  duplicate w@(WrapApply Right{}) = WrapApply (Right w)
  duplicate (WrapApply (Left fa)) = WrapApply (Left (extend (WrapApply . Left) fa))

instance ComonadApply f => ComonadApply (WrappedApply f)
  
-- | A variant of '<.>' with the arguments reversed.
(<..>) :: FunctorApply w => w a -> w (a -> b) -> w b
(<..>) = liftF2 (flip id)
{-# INLINE (<..>) #-}

-- | Lift a binary function into a comonad with zipping
liftF2 :: FunctorApply w => (a -> b -> c) -> w a -> w b -> w c
liftF2 f a b = f <$> a <.> b
{-# INLINE liftF2 #-}

-- | Lift a ternary function into a comonad with zipping
liftF3 :: FunctorApply w => (a -> b -> c -> d) -> w a -> w b -> w c -> w d
liftF3 f a b c = f <$> a <.> b <.> c
{-# INLINE liftF3 #-}

{- | 

A strong lax symmetric semi-monoidal comonad. As such an instance of 
'ComonadApply' is required to satisfy:

> extract (a <.> b) = extract a (extract b)

This class is based on ComonadZip from \"The Essence of Dataflow Programming\" 
by Tarmo Uustalu and Varmo Vene, but adapted to fit the programming style of
Control.Applicative. 'Applicative' can be seen as a similar law over and above 
FunctorApply that:

> pure (a b) = pure a <.> pure b

-}

class (Comonad w, FunctorApply w) => ComonadApply w
-- | Only requires a Semigroup, but no such class exists
instance Monoid m => ComonadApply ((,)m)
-- | Only requires a Semigroup, but no such class exists
instance Monoid m => ComonadApply ((->)m)
instance ComonadApply Identity
instance ComonadApply w => ComonadApply (IdentityT w)

-- | Lift a binary function into a comonad with zipping
liftW2 :: ComonadApply w => (a -> b -> c) -> w a -> w b -> w c
liftW2 = liftF2
{-# INLINE liftW2 #-}

-- | Lift a ternary function into a comonad with zipping
liftW3 :: ComonadApply w => (a -> b -> c -> d) -> w a -> w b -> w c -> w d
liftW3 = liftF3
{-# INLINE liftW3 #-}

-- | The 'Cokleisli' 'Arrow's of a given 'Comonad'
newtype Cokleisli w a b = Cokleisli { runCokleisli :: w a -> b }

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

instance ComonadApply w => ArrowLoop (Cokleisli w) where
  loop (Cokleisli f) = Cokleisli (fst . wfix . extend f') where 
    f' wa wb = f ((,) <$> wa <.> (snd <$> wb))

-- Cokleisli arrows are actually just a special case of a reader monad:

instance Functor (Cokleisli w a) where
  fmap f (Cokleisli g) = Cokleisli (f . g)

instance FunctorApply (Cokleisli w a) where
  Cokleisli f <.> Cokleisli a = Cokleisli (\w -> (f w) (a w))

instance Applicative (Cokleisli w a) where
  pure = Cokleisli . const
  Cokleisli f <*> Cokleisli a = Cokleisli (\w -> (f w) (a w))

instance Monad (Cokleisli w a) where
  return = Cokleisli . const
  Cokleisli k >>= f = Cokleisli $ \w -> runCokleisli (f (k w)) w

