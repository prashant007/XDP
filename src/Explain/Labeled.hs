module Explain.Labeled where

import Semiring.Semiring

import Data.Function(on)

data Labeled a = Label String a 


unlabel :: Labeled a -> a
unlabel (Label _ x) = x


firstLabel :: (a -> a -> a) -> Labeled a -> Labeled a -> Labeled a
firstLabel f (Label l x) (Label _ y) = Label l (f x y)


instance Show a => Show (Labeled a) where
  show (Label l x) = l++":"++show x

instance Functor Labeled where
  fmap f (Label l x) = Label l (f x)

instance (Eq a,Num a) => Eq (Labeled a) where
  (==) = (==) `on` unlabel

instance (Ord a,Num a) => Ord (Labeled a) where
  (<=) = (<=) `on` unlabel

instance Num a => Num (Labeled a) where
  (+) = firstLabel (+)
  (*) = firstLabel (*)
  (-) = firstLabel (-)
  negate = fmap negate
  abs    = fmap negate
  signum = fmap signum
  fromInteger x = Label "" (fromInteger x)

instance Semiring a => Semiring (Labeled a) where
  zero = Label "" zero
  one  = Label "" one
  Label l x <+> Label _ y = Label l (x <+> y)
  Label l x <.> Label _ y = Label l (x <.> y)

-- instance Eq a => Eq (Labeled a) where
--   Label _ x == Label _ y = x == y 

-- instance Ord a => Ord (Labeled a) where
--    Label _ x <= Label _ y = x <= y 

-- instance Functor Labeled where
--   fmap f (Label s x) = Label s (f x) 


-- instance Applicative Labeled where
--   pure = Label "" 
--   Label s1 f <*> Label s2 x = Label s1 (f x)


-- instance Num a => Num (Labeled a) where
--   Label s1 x + Label s2 y = Label s1 (x+y)
--   Label s1 x * Label s2 y = Label s1 (x*y)
--   Label s1 x - Label s2 y = Label s1 (x-y)
--   negate = fmap negate
--   abs    = fmap negate
--   signum = fmap signum
--   fromInteger x = Label "" (fromInteger x)
