  {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Examples.KnapSack where


-- Haskell imports
import Data.List (nub)
import Data.Maybe (fromJust)
import Control.Monad.Identity

-- DP imports
import DPSolver.DP
import DPSolver.Solvers
import Semiring.Semiring

-- Local imports
import Semiring.Large
import Semiring.View
import Semiring.Small

import Explain.Decomposed
import Explain.Labeled
import Explain.Dominators

import Data.List (sortBy)
import Data.Function (on)

type Index   = Int  
type Capacity = Double 
type Value  = Double
type Table v = [(Capacity,v)]
type Path  l = View l [Int]

class Semiring r => KS v r where
  result :: (Index,v) -> r

  ks :: Table v -> DP (Int,Capacity) r 
  ks t (0,_) = one
  ks t (_,0) = one 
  ks t (k,c) 
    | ck > c    = memo(k-1,c)
    | otherwise = ((inj.result) (k-1,vk) <.> memo (k-1,c-ck)) <+> memo(k-1,c)
    where (ck,vk) = t !! (k-1)

  knapSack :: Table v -> Capacity -> r
  knapSack t b = runDP (ks t) (length t,b)


-- (1) length, non-decomposed
--
instance KS Double (Small Double) where
  result (_,l) = Finite' l

-- (2) path, non-decomposed
--
instance KS Double (Path (Small Double)) where
  result (e,l) = View (Finite' l) [e]

instance KS [Double] (Small (Decomposed Double)) where
  result (_,l) = Finite' (Values l)

-- instance SP [Double] (Path DOUBLES) where
instance KS [Double] (Path (Small (Decomposed Double))) where
  result (e,l) = View (Finite' (Values l)) [e]

instance Decompose (Small (Decomposed Double)) Double where
  dec NegInfinity = Values []
  dec (Finite' vs) = vs
  -- lift = Finite
  supportive _ x = x < 0


capacity1 :: [Capacity]
capacity1 = [30,40,20,60,70,30,50]

values1 :: [Value]
values1   = [7 ,9 ,5 ,12,12,6 ,12]

values2 :: [[Double]]
values2  = [[7,3,2,0,-5],[5,2,4,0,-2],[1,0,0,4,0],[15,0,1,0,-4],[7.5,0.5,7,0,-3],[3,0,2,2,-1],[5,3,6,0,-2]]

capacityVal:: [(Capacity,Value)]
capacityVal = zip capacity1 values1

capacityDecompVal:: [(Capacity,[Value])]
capacityDecompVal = zip capacity1 values2

ks1 = knapSack capacityVal 150 :: Small Double
ks2 = knapSack capacityVal 150 :: Path (Small Double)
ks3 = knapSack capacityDecompVal 150 :: Small (Decomposed Double)
ks4 = knapSack capacityDecompVal 150 :: Path (Small (Decomposed Double))

k  = ks3 -- [20.0,8.0,14.0,2.0,-10.0]
k' = (Finite' . Values) [15.5,3.5,15,2,-6] -- [28,5,7,4,-11] 

-- [28,5,7,4,-11] for (30,40,20,60) ["Shell","BMW","WHO","Lufthansa"]
-- [15.5,3.5,15,2,-6] for (70,30,50) ["PepsiCo","Tesla","Dow Chemicals"]

categories = ["Actual Amount","Future Benefits","Inhouse Prod.","Govt. Fund.","Taxes"]

domKS = explainWith categories k k' 

type Path1  l = View1 l [Index]

instance KS Double (Path1 (Small Double)) where
  result (e,l) = V1 (Finite' l) (Just [e])
