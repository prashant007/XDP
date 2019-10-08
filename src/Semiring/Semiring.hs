module Semiring.Semiring where

import Data.Function (on)

infixl 9 <.>
infixl 8 <+>

class Semiring a where 
  zero, one :: a 
  (<+>), (<.>) :: a -> a -> a 

cartesian as bs = [(a,b) | a <- as, b <- bs] 

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f x = (<*>) (fmap f x)


sconcat :: Semiring a => [a] -> a
sconcat [] = zero 
sconcat (x:xs) = x <+> sconcat xs


