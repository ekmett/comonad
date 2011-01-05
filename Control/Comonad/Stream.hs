-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Stream
-- Copyright   :  (C) 2008-2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
-- 
-- The f-branching stream comonad, aka the cofree comonad for a Functor f.
----------------------------------------------------------------------------
module Control.Comonad.Stream
  ( Stream
  , heads
  , tails
  , unfolds
  ) where

import Control.Applicative
import Control.Comonad
import Data.Foldable
import Data.Monoid
import Data.Traversable
-- import Data.Data
-- import Data.Typeable
-- import Text.Read
-- import Text.Show

data Stream f a = Stream a (f (Stream f a))

instance Functor f => Functor (Stream f) where
  fmap f (Stream a as) = Stream (f a) (fmap f <$> as)

instance Functor f => Comonad (Stream f) where
  extract (Stream a _) = a
  duplicate aas = Stream aas (duplicate <$> tails aas)
  extend f aas = Stream (f aas) (extend f <$> tails aas)

instance Foldable f => Foldable (Stream f) where
  foldMap f (Stream a as) = f a `mappend` foldMap (foldMap f) as

instance Traversable f => Traversable (Stream f) where
  traverse f (Stream a as) = Stream <$> f a <*> traverse (traverse f) as

heads :: Stream f a -> a
heads (Stream a _) = a

tails :: Stream f a -> f (Stream f a)
tails (Stream _ as) = as

unfolds :: Functor f => (a -> (b, f a)) -> a -> Stream f b
unfolds f a = Stream h (unfolds f <$> t)
  where 
    (h, t) = f a

{-

instance Typeable1 f => Typeable1 (Stream f) where
  typeOf1 tfa = mkTyConApp streamTyCon [typeOf1 (undefined `asArgsType` tfa)]
    where 
      asArgsType :: f a -> t f a -> f a
      asArgsType = const

streamTyCon :: TyCon
streamTyCon = mkTyCon "Control.Comonad.Stream.Stream"
{-# NOINLINE streamTyCon #-}

consConstr :: Constr
consConstr = mkConstr streamDataType "Stream" [] Prefix
{-# NOINLINE consConstr #-}

streamDataType :: DataType
streamDataType = mkDataType "Control.Comonad.Stream.Stream" [consConstr]
{-# NOINLINE streamDataType #-}

-- Safe UndecidableInstances

instance (Show a, Show (f (Stream f a))) => Show (Stream f a) where
  showsPrec d (a :< as) = showParen (d > 3) $
    showsPrec 4 a . 
    showString " :< " . 
    showsPrec 3 as

instance (Read a, Read (f (Stream f a))) => Read (Stream f a) where
  readPrec = parens $ prec 3 $ do
    a <- step readPrec
    Symbol ":<" <- lexP
    as <- step readPrec
    return (a :< as)
      
instance (Eq a, Eq (f (Stream f a))) => Eq (Stream f a) where
  (a :< as) == (b :< bs) = a == b && as == bs

instance (Ord a, Ord (f (Stream f a))) => Ord (Stream f a) where
  compare (a :< as) (b :< bs) = case compare a b of
    LT -> LT
    EQ -> compare as bs
    GT -> GT

instance (Typeable1 f, Data (f (Stream f a)), Data a) => Data (Stream f a) where
  gfoldl f z (a :< as) = z (:<) `f` a `f` as
  toConstr _ = consConstr
  gunfold k z c = case constrIndex c of
    1 -> k (k (z (:<)))
    _ -> error "gunfold"
  dataTypeOf _ = streamDataType
  dataCast1 f = gcast1 f
-}
