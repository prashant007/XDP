{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Semiring.Large where

import Semiring.Semiring
import Text.Printf
import Semiring.View 
-- ======================
-- Min-Plus / Tropical
--
data Large a = Finite a | Infinity deriving (Eq,Ord)


instance Functor Large where
  fmap f (Finite x) = Finite (f x)
  fmap _ Infinity   = Infinity

instance Applicative Large where
  pure = Finite
  Finite f <*> Finite x = Finite (f x)
  _        <*> _        = Infinity

instance Num a => Num (Large a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  negate = fmap negate
  abs    = fmap negate
  signum = fmap signum
  fromInteger = Finite . fromInteger

instance (Num a,Ord a) => Semiring (Large a) where
  {zero = Infinity; one = Finite 0; (<+>) = min; (<.>) = (+)}


-- Selector instances for View semiring
--
instance Ord a => Selector (Large a) where
  first = (<=)

instance Show a => Show (Large a) where
  show Infinity   = "infinity"
  show (Finite a) = show a