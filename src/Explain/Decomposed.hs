module Explain.Decomposed where
import Data.Function (on)

newtype Decomposed a = Values {values :: [a]} deriving Show

instance (Eq a,Num a) => Eq (Decomposed a) where
  (==) = (==) `on` sum.values

instance (Ord a,Num a) => Ord (Decomposed a) where
  (<=) = (<=) `on` sum.values


pad :: Num a => Decomposed a -> Decomposed a -> a -> (Decomposed a,Decomposed a)
pad (Values xs) (Values ys) u = (Values xs1,Values ys1)
  where
    (xs1,ys1) = pad' xs ys u 


pad' :: Num a => [a] -> [a] -> a ->  ([a],[a])
pad' as bs u 
    | ld == 0   = (as,bs)
    | ld > 0    = (as,bs++zs)
    | otherwise = (as++zs,bs) 
  where
    zs    = replicate (absld) u 
    absld = abs ld 
    ld    = length as - length bs 


-- instance Show a => Show (Decomposed a) where
--   show (Decomposed d) = show d  

sumList :: Num a => [a] -> a 
sumList [] = 0
sumList (x:xs) = x + sumList xs 


liftDecomp :: Num a => (a -> a -> a) -> a -> Decomposed a -> Decomposed a -> Decomposed a 
liftDecomp f u x y = (<*>) (fmap f x1) y1
  where
    (x1,y1) = pad x y u 


instance Functor Decomposed where
  fmap f (Values d) = Values (map f d)

instance Applicative Decomposed where
  pure x = Values [x]
  (<*>) (Values fs) (Values vs) 
      = Values $ zipWith (\f v -> f v) fs vs  

instance Num a => Num (Decomposed a) where
  (+) = liftDecomp (+) 0 
  (*) = liftDecomp (*) 1 
  (-) = liftDecomp (-) 0
  negate = fmap negate
  abs    = fmap negate
  signum = fmap signum
  fromInteger x = Values $ [fromInteger x]



