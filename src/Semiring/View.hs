module Semiring.View where

import Semiring.Semiring


class Selector a where
  first :: a -> a -> Bool

data View a b = View a b deriving Eq 

instance (Show a, Show b) => Show (View a b) where
  show (View x y) = show x ++ " <~ " ++ show y


instance (Eq a,Eq b,Selector a,Semiring a,Monoid b) => Semiring (View a b) where
  zero = View zero mempty
  one  = View one mempty
  l@(View x _) <+> r@(View y _) = if first x y then l else r
  l@(View x a) <.> r@(View y b)
    | l == zero || r == zero = zero
    | otherwise = View (x <.> y) (mappend a b)


data View1 a b = V1 a (Maybe b) deriving (Show,Eq) 

instance (Eq a,Eq b,Selector a,Semiring a,Monoid b) => Semiring (View1 a b) where
  zero = V1 zero Nothing
  one  = V1 one (Just mempty)
  l@(V1 x _) <+> r@(V1 y _) = if first x y then l else r
  l@(V1 x a) <.> r@(V1 y b)
    | l == zero || r == zero = zero
    | otherwise = V1 (x <.> y) (mappend a b)
