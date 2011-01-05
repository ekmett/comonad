{-# LANGUAGE TypeOperators, StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}
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
--
----------------------------------------------------------------------------
module Control.Comonad.Stream
  ( Stream
  , tails
  , unfolds
  ) where

import Control.Comonad
import Data.Data
import Data.Monoid
import Data.Typeable
import Control.Applicative
import Data.Foldable
import Data.Traversable

infixl 3 :<

data Stream f a = a :< f (Stream f a)

instance Functor f => Functor (Stream f) where
    fmap f (a :< as) = f a :< fmap f <$> as

instance Functor f => Comonad (Stream f) where
    extract (a :< _) = a
    duplicate aas@(_ :< as) = aas :< duplicate <$> as
    extend f aas@(_ :< as) = f aas :< extend f <$> as

instance Foldable f => Foldable (Stream f) where
    foldMap f (a :< as) = f a `mappend` foldMap (foldMap f) as

instance Traversable f => Traversable (Stream f) where
    traverse f (a :< as) = (:<) <$> f a <*> traverse (traverse f) as

deriving instance (Show a, Show (f (Stream f a))) => Show (Stream f a)

tails :: Stream f a -> f (Stream f a)
tails (_ :< as) = as

unfolds :: Functor f => (a -> (b, f a)) -> a -> Stream f b
unfolds f a = h :< unfolds f <$> t 
  where 
    (h, t) = f a

instance Typeable1 f => Typeable1 (Stream f) where
    typeOf1 tfa = mkTyConApp streamTyCon [typeOf1 (undefined `asArgsType` tfa)]
        where asArgsType :: f a -> t f a -> f a
              asArgsType = const

streamTyCon :: TyCon
streamTyCon = mkTyCon "Control.Comonad.Stream.Stream"
{-# NOINLINE streamTyCon #-}

consConstr :: Constr
consConstr = mkConstr streamDataType "(:<)" [] Infix
{-# NOINLINE consConstr #-}

streamDataType :: DataType
streamDataType = mkDataType "Control.Comonad.Stream.Stream" [consConstr]
{-# NOINLINE streamDataType #-}

instance (Typeable1 f, Data (f (Stream f a)), Data a) => Data (Stream f a) where
    gfoldl f z (a :< as) = z (:<) `f` a `f` as
    toConstr _ = consConstr
    gunfold k z c = case constrIndex c of
        1 -> k (k (z (:<)))
        _ -> error "gunfold"
    dataTypeOf _ = streamDataType
    dataCast1 f = gcast1 f
