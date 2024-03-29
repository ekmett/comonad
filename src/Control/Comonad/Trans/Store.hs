{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Safe #-}

-- |
-- Copyright   :  (C) 2008-2021 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- The store comonad holds a constant value along with a modifiable /accessor/
-- function, which maps the /stored value/ to the /focus/.
--
-- This module defines the strict store (aka state-in-context/costate) comonad
-- transformer.
--
-- @stored value = (1, 5)@, @accessor = fst@, @resulting focus = 1@:
--
-- >>> :{
--  let
--    storeTuple :: Store (Int, Int) Int
--    storeTuple = Store fst (1, 5)
-- :}
--
-- Add something to the focus:
--
-- >>> :{
--  let
--    addToFocus :: Int -> Store (Int, Int) Int -> Int
--    addToFocus x wa = x + extract wa
-- :}
--
-- >>> :{
--   let
--     added3 :: Store (Int, Int) Int
--     added3 = extend (addToFocus 3) storeTuple
-- :}
--
-- The focus of added3 is now @1 + 3 = 4@. However, this action changed only
-- the accessor function and therefore the focus but not the stored value:
--
-- >>> pos added3
-- (1,5)
--
-- >>> extract added3
-- 4
--
-- The strict store (state-in-context/costate) comonad transformer is subject
-- to the laws:
--
-- > x = seek (pos x) x
-- > y = pos (seek y x)
-- > seek y x = seek y (seek z x)
--
-- Thanks go to Russell O'Connor and Daniel Peebles for their help formulating
-- and proving the laws for this comonad transformer.

module Control.Comonad.Trans.Store
(
-- * The Store comonad
  Store, pattern Store, runStore
-- * The Store comonad transformer
, StoreT(..), runStoreT
-- * Operations
, pos
, seek, seeks
, peek, peeks
, experiment
) where

import Control.Comonad
import Control.Comonad.Hoist.Class
import Control.Comonad.Trans.Class
import Data.Functor.Identity
import GHC.Generics

-- $setup
-- >>> import Control.Comonad
-- >>> import Data.Tuple (swap)

type Store s = StoreT s Identity

-- | Create a Store using an accessor function and a stored value
pattern Store :: (s -> a) -> s -> Store s a
pattern Store f s = StoreT (Identity f) s

runStore :: Store s a -> (s -> a, s)
runStore = \(StoreT (Identity f) s) -> (f, s)
{-# inline runStore #-}

data StoreT s w a = StoreT (w (s -> a)) s
  deriving (Generic, Generic1)

runStoreT :: StoreT s w a -> (w (s -> a), s)
runStoreT = \(StoreT wf s) -> (wf, s)
{-# inline runStoreT #-}

instance Functor w => Functor (StoreT s w) where
  fmap = \f (StoreT wf s) -> StoreT (fmap (f .) wf) s
  {-# inline fmap #-}

instance (ComonadApply w, Semigroup s) => ComonadApply (StoreT s w) where
  (<@>) = \(StoreT ff m) (StoreT fa n) -> StoreT ((<*>) <$> ff <@> fa) (m <> n)
  {-# inline (<@>) #-}

instance (Applicative w, Monoid s) => Applicative (StoreT s w) where
  pure = \a -> StoreT (pure (const a)) mempty
  {-# inline pure #-}
  (<*>) = \(StoreT ff m) (StoreT fa n) -> StoreT ((<*>) <$> ff <*> fa) (mappend m n)
  {-# inline (<*>) #-}

instance Comonad w => Comonad (StoreT s w) where
  duplicate = \(StoreT wf s) -> StoreT (extend StoreT wf) s
  extend = \f (StoreT wf s) -> StoreT (extend (\wf' s' -> f (StoreT wf' s')) wf) s
  extract = \(StoreT wf s) -> extract wf s
  {-# inline duplicate #-}
  {-# inline extend #-}
  {-# inline extract #-}

instance ComonadTrans (StoreT s) where
  lower = \(StoreT f s) -> fmap ($ s) f
  {-# inline lower #-}

instance ComonadHoist (StoreT s) where
  cohoist = \l (StoreT f s) -> StoreT (l f) s
  {-# inline cohoist #-}

-- | Read the stored value
--
-- >>> pos $ Store fst (1,5)
-- (1,5)
--
pos :: StoreT s w a -> s
pos = \(StoreT _ s) -> s
{-# inline pos #-}

-- | Set the stored value
--
-- >>> pos . seek (3,7) $ Store fst (1,5)
-- (3,7)
--
-- Seek satisfies the law
--
-- > seek s = peek s . duplicate
seek :: s -> StoreT s w a -> StoreT s w a
seek = \s ~(StoreT f _) -> StoreT f s
{-# inline seek #-}

-- | Modify the stored value
--
-- >>> pos . seeks swap $ Store fst (1,5)
-- (5,1)
--
-- Seeks satisfies the law
--
-- > seeks f = peeks f . duplicate
seeks :: (s -> s) -> StoreT s w a -> StoreT s w a
seeks = \f ~(StoreT g s) -> StoreT g (f s)
{-# inline seeks #-}

-- | Peek at what the current focus would be for a different stored value
--
-- Peek satisfies the law
--
-- > peek x . extend (peek y) = peek y
peek :: Comonad w => s -> StoreT s w a -> a
peek = \s (StoreT g _) -> extract g s
{-# inline peek #-}


-- | Peek at what the current focus would be if the stored value was
--   modified by some function
peeks :: Comonad w => (s -> s) -> StoreT s w a -> a
peeks = \f ~(StoreT g s) -> extract g (f s)
{-# inline peeks #-}

-- | Applies a functor-valued function to the stored value, and then uses the
--   new accessor to read the resulting focus.
--
--   >>> let f x = if x > 0 then Just (x^2) else Nothing
--   >>> experiment f $ Store (+1) 2
--   Just 5
--   >>> experiment f $ Store (+1) (-2)
--   Nothing
experiment :: (Comonad w, Functor f) => (s -> f s) -> StoreT s w a -> f a
experiment = \f (StoreT wf s) -> extract wf <$> f s
{-# inline experiment #-}
