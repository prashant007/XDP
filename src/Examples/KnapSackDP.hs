{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-- {-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}

module Examples.KnapSackDP where


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


type Name   = String 
type Weight = Double 
type Value  = Double
type Table l = [(Name,Weight,l)]
type Path  l = View l [Name]

class Semiring r => KS l r where
  result :: (Name,l) -> r

  ks :: Table l -> DP (Int,Weight) r 
  ks t (0,_) = one
  ks t (_,0) = one 
  ks t (k,b) 
    | wk > b    = memo(k-1,b)
    | otherwise = memo(k-1,b) <+> ((inj.result) (nk,vk) <.> memo (k-1,b-wk))
    where
      tk@(nk,wk,vk) = t !! (k-1)

  knapSack :: Table l -> Weight -> r
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


names1   = ["Shell","BMW","WHO","Lufthansa","PepsiCo","Tesla","Dow Chemicals"]

weights1,values1 :: [Double]
weights1 = [30,40,20,60,70,30,50]
values1  = [7 ,9 ,5 ,12,12,6 ,12]

values2 :: [[Double]]
values2  = [[7,3,2,0,-5],[5,2,4,0,-2],[1,0,0,4,0],[15,0,1,0,-4],[7.5,0.5,7,0,-3],[3,0,2,2,-1],[5,3,6,0,-2]]

triple1:: [(String,Double,Double)]
triple1 = zip3 names1 weights1 values1

triple2:: [(String,Double,[Double])]
triple2 = zip3 names1 weights1 values2


ks1 = knapSack triple1 150 :: Small Double
ks2 = knapSack triple1 150 :: Path (Small Double)
ks3 = knapSack triple2 150 :: Small (Decomposed Double)
ks4 = knapSack triple2 150 :: Path (Small (Decomposed Double))

k  = ks3 -- [20.0,8.0,14.0,2.0,-10.0]
k' = (Finite' . Values) [15.5,3.5,15,2,-6] -- [28,5,7,4,-11] 

-- [28,5,7,4,-11] for (30,40,20,60) ["Shell","BMW","WHO","Lufthansa"]
-- [15.5,3.5,15,2,-6] for (70,30,50) ["PepsiCo","Tesla","Dow Chemicals"]

categories = ["Actual Amount","Future Benefits","Inhouse Prod.","Govt. Fund.","Taxes"]

domKS = explainWith categories k k' 

type Path1  l = View1 l [Name]

instance KS Double (Path1 (Small Double)) where
  result (e,l) = V1 (Finite' l) (Just [e])

ks5 = knapSack triple1 150 :: Path1 (Small Double)
