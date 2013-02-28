{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
#endif
 -----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad
-- Copyright   :  (C) 2008-2012 Edward Kmett,
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
    Comonad(..)
  , liftW     -- :: Comonad w => (a -> b) -> w a -> w b
  , wfix      -- :: Comonad w => w (w a -> a) -> a
  , cfix      -- :: Comonad w => (w a -> a) -> w a
  , (=>=)
  , (=<=)
  , (<<=)
  , (=>>)
  -- * Combining Comonads
  , ComonadApply(..)
  , (<@@>)    -- :: ComonadApply w => w a -> w (a -> b) -> w b
  , liftW2    -- :: ComonadApply w => (a -> b -> c) -> w a -> w b -> w c
  , liftW3    -- :: ComonadApply w => (a -> b -> c -> d) -> w a -> w b -> w c -> w d
  -- * Cokleisli Arrows
  , Cokleisli(..)
  -- * Functors
  , Functor(..)
  , (<$>)     -- :: Functor f => (a -> b) -> f a -> f b
  , ($>)      -- :: Functor f => f a -> b -> f b
  ) where

-- import _everything_
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad (ap)
#if MIN_VERSION_base(4,7,0)
-- Control.Monad.Instances is empty
#else
import Control.Monad.Instances
#endif
import Control.Monad.Trans.Identity
import Data.Functor.Identity
import Data.List.NonEmpty hiding (map)
import Data.Semigroup hiding (Product)
import Data.Tree
import Prelude hiding (id, (.))
import Control.Monad.Fix
#if __GLASGOW_HASKELL__ >= 707
-- Data.Typeable is redundant
#else
import Data.Typeable
#endif

infixl 4 <@, @>, <@@>, <@>, $>
infixl 1 =>>
infixr 1 <<=, =<=, =>=

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

These are the default definitions of 'extend' and 'duplicate' and
the definition of 'liftW' respectively.

-}

class Functor w => Comonad w where
  -- |
  -- > extract . fmap f = f . extract
  extract :: w a -> a

  -- |
  -- > duplicate = extend id
  -- > fmap (fmap f) . duplicate = duplicate . fmap f
  duplicate :: w a -> w (w a)
  duplicate = extend id

  -- |
  -- > extend f = fmap f . duplicate
  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate


instance Comonad ((,)e) where
  duplicate p = (fst p, p)
  {-# INLINE duplicate #-}
  extract = snd
  {-# INLINE extract #-}

instance Monoid m => Comonad ((->)m) where
  duplicate f m = f . mappend m
  {-# INLINE duplicate #-}
  extract f = f mempty
  {-# INLINE extract #-}

instance Comonad Identity where
  duplicate = Identity
  {-# INLINE duplicate #-}
  extract = runIdentity
  {-# INLINE extract #-}

instance Comonad w => Comonad (IdentityT w) where
  extend f (IdentityT m) = IdentityT (extend (f . IdentityT) m)
  extract = extract . runIdentityT
  {-# INLINE extract #-}

instance Comonad Tree where
  duplicate w@(Node _ as) = Node w (map duplicate as)
  extract (Node a _) = a
  {-# INLINE extract #-}

instance Comonad NonEmpty where
  extend f w@ ~(_ :| aas) = f w :| case aas of
      []     -> []
      (a:as) -> toList (extend f (a :| as))
  extract ~(a :| _) = a
  {-# INLINE extract #-}

-- | @ComonadApply@ is to @Comonad@ like @Applicative@ is to @Monad@.
--
-- Mathematically, it is a strong lax symmetric semi-monoidal comonad on the
-- category @Hask@ of Haskell types. That it to say that @w@ is a strong lax
-- symmetric semi-monoidal functor on Hask, where both extract and duplicate are
-- symmetric monoidal natural transformations.
--
-- Laws:
--
-- > (.) <$> u <@> v <@> w = u <@> (v <@> w)
-- > extract p (extract q) = extract (p <@> q)
-- > duplicate (p <*> q) = (\r s -> fmap (r <@> s)) <@> duplicate q <*> duplicate q
--
-- If our type is both a ComonadApply and Applicative we further require
--
-- > (<*>) = (<@>)
--
-- Finally, if you choose to define ('<@') and ('@>'), the results of your
-- definitions should match the following laws:
--
-- > a @> b = const id <$> a <@> b
-- > a <@ b = const <$> a <@> b

class Comonad w => ComonadApply w where
  (<@>) :: w (a -> b) -> w a -> w b

  (@>) :: w a -> w b -> w b
  a @> b = const id <$> a <@> b

  (<@) :: w a -> w b -> w a
  a <@ b = const <$> a <@> b

instance Semigroup m => ComonadApply ((,)m) where
  (m, f) <@> (n, a) = (m <> n, f a)
  (m, a) <@  (n, _) = (m <> n, a)
  (m, _)  @> (n, b) = (m <> n, b)

instance ComonadApply NonEmpty where
  (<@>) = ap

instance Monoid m => ComonadApply ((->)m) where
  (<@>) = (<*>)
  (<@ ) = (<* )
  ( @>) = ( *>)

instance ComonadApply Identity where
  (<@>) = (<*>)
  (<@ ) = (<* )
  ( @>) = ( *>)

instance ComonadApply w => ComonadApply (IdentityT w) where
  IdentityT wa <@> IdentityT wb = IdentityT (wa <@> wb)

instance ComonadApply Tree where
  (<@>) = (<*>)
  (<@ ) = (<* )
  ( @>) = ( *>)

-- | A suitable default definition for 'fmap' for a 'Comonad'.
-- Promotes a function to a comonad.
--
-- > fmap f = liftW f = extend (f . extract)
liftW :: Comonad w => (a -> b) -> w a -> w b
liftW f = extend (f . extract)
{-# INLINE liftW #-}

-- | Comonadic fixed point à la Menendez
wfix :: Comonad w => w (w a -> a) -> a
wfix w = extract w (extend wfix w)

-- | Comonadic fixed point à la Orchard
cfix :: Comonad w => (w a -> a) -> w a
cfix f = fix (extend f)
{-# INLINE cfix #-}

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

-- | A variant of '<@>' with the arguments reversed.
(<@@>) :: ComonadApply w => w a -> w (a -> b) -> w b
(<@@>) = liftW2 (flip id)
{-# INLINE (<@@>) #-}

-- | Lift a binary function into a comonad with zipping
liftW2 :: ComonadApply w => (a -> b -> c) -> w a -> w b -> w c
liftW2 f a b = f <$> a <@> b
{-# INLINE liftW2 #-}

-- | Lift a ternary function into a comonad with zipping
liftW3 :: ComonadApply w => (a -> b -> c -> d) -> w a -> w b -> w c -> w d
liftW3 f a b c = f <$> a <@> b <@> c
{-# INLINE liftW3 #-}

-- | The 'Cokleisli' 'Arrow's of a given 'Comonad'
newtype Cokleisli w a b = Cokleisli { runCokleisli :: w a -> b }

#if __GLASGOW_HASKELL__ >= 707
-- instance Typeable (Cokleisli w) derived automatically
#else
#ifdef __GLASGOW_HASKELL__
instance Typeable1 w => Typeable2 (Cokleisli w) where
  typeOf2 twab = mkTyConApp cokleisliTyCon [typeOf1 (wa twab)]
        where wa :: Cokleisli w a b -> w a
              wa = undefined
#endif

cokleisliTyCon :: TyCon
#if MIN_VERSION_base(4,4,0)
cokleisliTyCon = mkTyCon3 "comonad" "Control.Comonad" "Cokleisli"
#else
cokleisliTyCon = mkTyCon "Control.Comonad.Cokleisli"
#endif
{-# NOINLINE cokleisliTyCon #-}
#endif

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
    f' wa wb = f ((,) <$> wa <@> (snd <$> wb))

instance Functor (Cokleisli w a) where
  fmap f (Cokleisli g) = Cokleisli (f . g)

instance Applicative (Cokleisli w a) where
  pure = Cokleisli . const
  Cokleisli f <*> Cokleisli a = Cokleisli (\w -> (f w) (a w))

instance Monad (Cokleisli w a) where
  return = Cokleisli . const
  Cokleisli k >>= f = Cokleisli $ \w -> runCokleisli (f (k w)) w

-- | Replace the contents of a functor uniformly with a constant value.
($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)
