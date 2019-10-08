module Explain.Principal where

import Data.Function (on)

import Explain.Decomposed (Decomposed(..))


-- Principal Value Decomposition
--
newtype Principal a = PValues {pvalues :: [a]}


asPrincipal :: Decomposed a -> Principal a 
asPrincipal (Values v) = PValues v 

fromPrincipal1 :: Principal a -> Decomposed a 
fromPrincipal1 (PValues v) = Values v 


asDecomposed :: Principal a -> Decomposed a
asDecomposed (PValues xs) = Values xs

class FromPrincipal f where
  fromPrincipal :: f (Principal a)  -> f (Decomposed a)


instance Show a => Show (Principal a) where
  show = show . pvalues

instance Functor Principal where
  fmap f (PValues ps) = PValues (map f ps)

instance Applicative Principal where
  pure x = PValues [x]
  PValues fs <*> PValues vs = PValues $ zipWith ($) fs vs

-- liftD :: Num a => (a -> a -> a) -> a -> Principal a -> Principal a -> Principal a
liftD :: (a -> a -> a) -> a -> Principal a -> Principal a -> Principal a
liftD f u x y = fmap f x1 <*> y1
           where (x1,y1) = mypad x y u

-- mypad :: Num a => Principal a -> Principal a -> a -> (Principal a,Principal a)
mypad :: Principal a -> Principal a -> a -> (Principal a,Principal a)
mypad (PValues xs) (PValues ys) u = (PValues xs1,PValues ys1)
    where (xs1,ys1) = mypad' xs ys u

-- mypad' :: Num a => [a] -> [a] -> a ->  ([a],[a])
mypad' :: [a] -> [a] -> a ->  ([a],[a])
mypad' as bs u | ld == 0   = (as,bs)
             | ld > 0    = (as,bs++zs)
             | otherwise = (as++zs,bs)
             where zs    = replicate (absld) u
                   absld = abs ld
                   ld    = length as - length bs


instance (Eq a,Num a) => Eq (Principal a) where
  -- PValues xs == PValues ys = sum xs == sum ys
  (==) = (==) `on` head.pvalues

instance (Ord a,Num a) => Ord (Principal a) where
  -- PValues xs <= PValues ys = sum xs <= sum ys
  (<=) = (<=) `on` head.pvalues

instance Num a => Num (Principal a) where
  (+) = liftD (+) 0
  (*) = liftD (*) 1
  (-) = liftD (-) 0
  negate = fmap negate
  abs    = fmap abs
  signum = fmap signum
  fromInteger x = PValues [fromInteger x]




