{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Semiring.Small where 
import Semiring.Semiring
import Semiring.View 
import Text.Printf


data Small a = NegInfinity | Finite' a deriving(Eq,Ord,Show) 

instance Functor Small where
  fmap f (Finite' d)     = Finite' (f d)
  fmap _ NegInfinity = NegInfinity

instance Applicative Small where
  pure = Finite'    
  Finite' f <*> m = fmap f m 
  _     <*> _ = NegInfinity


instance Num a => Num (Small a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  negate = fmap negate
  abs    = fmap negate
  signum = fmap signum
  fromInteger = Finite'. fromInteger

instance (Num a,Ord a) => Semiring (Small a) where
    {zero = NegInfinity; one  = Finite' 0 ; (<+>) = max ; (<.>) = (+)}


newtype VProb = VProb Double
    deriving (Eq,Show,Num,Ord,Fractional) 

instance Semiring VProb where
    zero = 0
    one  = 1
    (<+>)  = max  
    (<.>)  = (*)  

instance Ord a => Selector (Small a) where
  first = (>)

instance Selector VProb where
  first = (>)
