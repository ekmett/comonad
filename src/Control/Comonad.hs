{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}

-- |
-- Copyright   :  (C) 2008-2021 Edward Kmett,
--                (C) 2004 Dave Menendez
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
module Control.Comonad
(
-- * Comonads
  Comonad(..)
, liftW     -- :: Comonad w => (a -> b) -> w a -> w b
, wfix      -- :: Comonad w => w (w a -> a) -> a
, cfix      -- :: Comonad w => (w a -> a) -> w a
, kfix      -- :: ComonadApply w => w (w a -> a) -> w a
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

import Data.Functor
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad.Fix
import Control.Monad.Trans.Identity
import Data.Functor.Identity
import qualified Data.Functor.Sum as FSum
import Data.List.NonEmpty hiding (map)
import Data.Semigroup hiding (Product)
import Data.Tagged
import GHC.Generics
import Prelude hiding (id, (.))

#ifdef MIN_VERSION_containers
import Data.Tree
#endif

infixl 4 <@, @>, <@@>, <@>
infixl 1 =>>
infixr 1 <<=, =<=, =>=

{- |

There are two ways to define a comonad:

I. Provide definitions for 'extract' and 'extend'
satisfying these laws:

@
'extend' 'extract'      = 'id'
'extract' . 'extend' f  = f
'extend' f . 'extend' g = 'extend' (f . 'extend' g)
@

In this case, you may simply set 'fmap' = 'liftW'.

These laws are directly analogous to the laws for monads
and perhaps can be made clearer by viewing them as laws stating
that Cokleisli composition must be associative, and has extract for
a unit:

@
f '=>=' 'extract'   = f
'extract' '=>=' f   = f
(f '=>=' g) '=>=' h = f '=>=' (g '=>=' h)
@

II. Alternately, you may choose to provide definitions for 'fmap',
'extract', and 'duplicate' satisfying these laws:

@
'extract' . 'duplicate'      = 'id'
'fmap' 'extract' . 'duplicate' = 'id'
'duplicate' . 'duplicate'    = 'fmap' 'duplicate' . 'duplicate'
@

In this case you may not rely on the ability to define 'fmap' in
terms of 'liftW'.

You may of course, choose to define both 'duplicate' /and/ 'extend'.
In that case you must also satisfy these laws:

@
'extend' f  = 'fmap' f . 'duplicate'
'duplicate' = 'extend' id
'fmap' f    = 'extend' (f . 'extract')
@

These are the default definitions of 'extend' and 'duplicate' and
the definition of 'liftW' respectively.

-}

class Functor w => Comonad w where
  -- |
  -- @
  -- 'extract' . 'fmap' f = f . 'extract'
  -- @
  extract :: w a -> a

  -- |
  -- @
  -- 'duplicate' = 'extend' 'id'
  -- 'fmap' ('fmap' f) . 'duplicate' = 'duplicate' . 'fmap' f
  -- @
  duplicate :: w a -> w (w a)
  duplicate = extend id
  {-# inline duplicate #-}

  -- |
  -- @
  -- 'extend' f = 'fmap' f . 'duplicate'
  -- @
  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate
  {-# inline extend #-}

  {-# MINIMAL extract, (duplicate | extend) #-}

instance Comonad ((,) e) where
  duplicate p = (fst p, p)
  {-# inline duplicate #-}
  extract = snd
  {-# inline extract #-}

instance Comonad (Arg e) where
  duplicate w@(Arg a _) = Arg a w
  {-# inline duplicate #-}
  extend f w@(Arg a _) = Arg a (f w)
  {-# inline extend #-}
  extract (Arg _ b) = b
  {-# inline extract #-}

instance Monoid m => Comonad ((->)m) where
  duplicate f m = f . mappend m
  {-# inline duplicate #-}
  extract f = f mempty
  {-# inline extract #-}

instance Comonad Identity where
  duplicate = Identity
  {-# inline duplicate #-}
  extract = runIdentity
  {-# inline extract #-}

-- $
-- The variable `s` can have any kind.
-- For example, here it has kind `Bool`:
-- >>> :set -XDataKinds
-- >>> import Data.Tagged
-- >>> extract (Tagged 42 :: Tagged 'True Integer)
-- 42
instance Comonad (Tagged s) where
  duplicate = Tagged
  {-# inline duplicate #-}
  extract = unTagged
  {-# inline extract #-}

instance Comonad w => Comonad (IdentityT w) where
  extend f (IdentityT m) = IdentityT (extend (f . IdentityT) m)
  {-# inline extend #-}
  extract = extract . runIdentityT
  {-# inline extract #-}

#ifdef MIN_VERSION_containers
instance Comonad Tree where
  duplicate w@(Node _ as) = Node w (map duplicate as)
  {-# inline duplicate #-}
  extract (Node a _) = a
  {-# inline extract #-}
#endif

instance Comonad NonEmpty where
  extend f w@(~(_ :| aas)) =
    f w :| case aas of
      []     -> []
      (a:as) -> toList (extend f (a :| as))
  {-# inline extend #-}
  extract ~(a :| _) = a
  {-# inline extract #-}

coproduct :: (f a -> b) -> (g a -> b) -> FSum.Sum f g a -> b
coproduct f _ (FSum.InL x) = f x
coproduct _ g (FSum.InR y) = g y
{-# inline coproduct #-}

instance (Comonad f, Comonad g) => Comonad (FSum.Sum f g) where
  extend f = coproduct
               (FSum.InL . extend (f . FSum.InL))
               (FSum.InR . extend (f . FSum.InR))
  {-# inline extend #-}
  extract = coproduct extract extract
  {-# inline extract #-}


-- | @ComonadApply@ is to @Comonad@ like @Applicative@ is to @Monad@.
--
-- Mathematically, it is a strong lax symmetric semi-monoidal comonad on the
-- category @Hask@ of Haskell types. That it to say that @w@ is a strong lax
-- symmetric semi-monoidal functor on Hask, where both 'extract' and 'duplicate' are
-- symmetric monoidal natural transformations.
--
-- Laws:
--
-- @
-- ('.') '<$>' u '<@>' v '<@>' w = u '<@>' (v '<@>' w)
-- 'extract' (p '<@>' q) = 'extract' p ('extract' q)
-- 'duplicate' (p '<@>' q) = ('<@>') '<$>' 'duplicate' p '<@>' 'duplicate' q
-- @
--
-- If our type is both a 'ComonadApply' and 'Applicative' we further require
--
-- @
-- ('<*>') = ('<@>')
-- @
--
-- Finally, if you choose to define ('<@') and ('@>'), the results of your
-- definitions should match the following laws:
--
-- @
-- a '@>' b = 'const' 'id' '<$>' a '<@>' b
-- a '<@' b = 'const' '<$>' a '<@>' b
-- @

class Comonad w => ComonadApply w where
  (<@>) :: w (a -> b) -> w a -> w b
  default (<@>) :: Applicative w => w (a -> b) -> w a -> w b
  (<@>) = (<*>)

  (@>) :: w a -> w b -> w b
  a @> b = id <$ a <@> b

  (<@) :: w a -> w b -> w a
  a <@ b = const <$> a <@> b
  {-# inline (<@>) #-}
  {-# inline (@>) #-}
  {-# inline (<@) #-}

instance Semigroup m => ComonadApply ((,)m) where
  (m, f) <@> (n, a) = (m <> n, f a)
  (m, a) <@  (n, _) = (m <> n, a)
  (m, _)  @> (n, b) = (m <> n, b)
  {-# inline (<@>) #-}
  {-# inline (@>) #-}
  {-# inline (<@) #-}

instance ComonadApply NonEmpty where
  (<@>) = (<*>)
  (<@ ) = (<* )
  ( @>) = ( *>)
  {-# inline (<@>) #-}
  {-# inline (@>) #-}
  {-# inline (<@) #-}

instance Monoid m => ComonadApply ((->)m) where
  (<@>) = (<*>)
  (<@ ) = (<* )
  ( @>) = ( *>)
  {-# inline (<@>) #-}
  {-# inline (@>) #-}
  {-# inline (<@) #-}

instance ComonadApply Identity where
  (<@>) = (<*>)
  (<@ ) = (<* )
  ( @>) = ( *>)
  {-# inline (<@>) #-}
  {-# inline (@>) #-}
  {-# inline (<@) #-}

instance ComonadApply w => ComonadApply (IdentityT w) where
  IdentityT wa <@> IdentityT wb = IdentityT (wa <@> wb)
  {-# inline (<@>) #-}

#ifdef MIN_VERSION_containers
instance ComonadApply Tree where
  (<@>) = (<*>)
  (<@ ) = (<* )
  ( @>) = ( *>)
  {-# inline (<@>) #-}
  {-# inline (@>) #-}
  {-# inline (<@) #-}
#endif

-- | A suitable default definition for 'fmap' for a 'Comonad'.
-- Promotes a function to a comonad.
--
-- You can only safely use 'liftW' to define 'fmap' if your 'Comonad'
-- defines 'extend', not just 'duplicate', since defining
-- 'extend' in terms of duplicate uses 'fmap'!
--
-- @
-- 'fmap' f = 'liftW' f = 'extend' (f . 'extract')
-- @
liftW :: Comonad w => (a -> b) -> w a -> w b
liftW f = extend (f . extract)
{-# inline liftW #-}

-- | Comonadic fixed point à la David Menendez
wfix :: Comonad w => w (w a -> a) -> a
wfix w = extract w (extend wfix w)

-- | Comonadic fixed point à la Dominic Orchard
cfix :: Comonad w => (w a -> a) -> w a
cfix f = fix (extend f)
{-# inline cfix #-}

-- | Comonadic fixed point à la Kenneth Foner:
--
-- This is the @evaluate@ function from his <https://www.youtube.com/watch?v=F7F-BzOB670 "Getting a Quick Fix on Comonads"> talk.
kfix :: ComonadApply w => w (w a -> a) -> w a
kfix w = fix $ \u -> w <@> duplicate u
{-# inline kfix #-}

-- | 'extend' with the arguments swapped. Dual to '>>=' for a 'Monad'.
(=>>) :: Comonad w => w a -> (w a -> b) -> w b
(=>>) = flip extend
{-# inline (=>>) #-}

-- | 'extend' in operator form
(<<=) :: Comonad w => (w a -> b) -> w a -> w b
(<<=) = extend
{-# inline (<<=) #-}

-- | Right-to-left 'Cokleisli' composition
(=<=) :: Comonad w => (w b -> c) -> (w a -> b) -> w a -> c
f =<= g = f . extend g
{-# inline (=<=) #-}

-- | Left-to-right 'Cokleisli' composition
(=>=) :: Comonad w => (w a -> b) -> (w b -> c) -> w a -> c
f =>= g = g . extend f
{-# inline (=>=) #-}

-- | A variant of '<@>' with the arguments reversed.
(<@@>) :: ComonadApply w => w a -> w (a -> b) -> w b
(<@@>) = liftW2 (flip id)
{-# inline (<@@>) #-}

-- | Lift a binary function into a 'Comonad' with zipping
liftW2 :: ComonadApply w => (a -> b -> c) -> w a -> w b -> w c
liftW2 f a b = f <$> a <@> b
{-# inline liftW2 #-}

-- | Lift a ternary function into a 'Comonad' with zipping
liftW3 :: ComonadApply w => (a -> b -> c -> d) -> w a -> w b -> w c -> w d
liftW3 f a b c = f <$> a <@> b <@> c
{-# inline liftW3 #-}

-- | The 'Cokleisli' 'Arrow's of a given 'Comonad'
newtype Cokleisli w a b = Cokleisli { runCokleisli :: w a -> b }
  deriving (Generic, Generic1)

instance Comonad w => Category (Cokleisli w) where
  id = Cokleisli extract
  {-# inline id #-}
  Cokleisli f . Cokleisli g = Cokleisli (f =<= g)
  {-# inline (.) #-}

instance Comonad w => Arrow (Cokleisli w) where
  arr f = Cokleisli (f . extract)
  {-# inline arr #-}
  first f = f *** id
  {-# inline first #-}
  second f = id *** f
  {-# inline second #-}
  Cokleisli f *** Cokleisli g = Cokleisli (f . fmap fst &&& g . fmap snd)
  {-# inline (***) #-}
  Cokleisli f &&& Cokleisli g = Cokleisli (f &&& g)
  {-# inline (&&&) #-}

instance Comonad w => ArrowApply (Cokleisli w) where
  app = Cokleisli $ \w -> runCokleisli (fst (extract w)) (snd <$> w)
  {-# inline app #-}

instance Comonad w => ArrowChoice (Cokleisli w) where
  left = leftApp
  {-# inline left #-}

instance ComonadApply w => ArrowLoop (Cokleisli w) where
  loop (Cokleisli f) = Cokleisli (fst . wfix . extend f') where
    f' wa wb = f ((,) <$> wa <@> (snd <$> wb))
  {-# inline loop #-}

instance Functor (Cokleisli w a) where
  fmap f (Cokleisli g) = Cokleisli (f . g)
  {-# inline fmap #-}

instance Applicative (Cokleisli w a) where
  pure = Cokleisli . const
  {-# inline pure #-}
  Cokleisli f <*> Cokleisli a = Cokleisli (\w -> f w (a w))
  {-# inline (<*>) #-}

instance Monad (Cokleisli w a) where
  Cokleisli k >>= f = Cokleisli $ \w -> runCokleisli (f (k w)) w
  {-# inline (>>=) #-}
