{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveAnyClass #-}

module Examples.HMM_V1 where
-- Haskell imports
import Data.List (nub)
import Data.Maybe (fromJust)
import Control.Monad.Identity

-- DP imports
import DPSolver.DP
import DPSolver.Solvers
import Semiring.Semiring

-- Local imports
import Semiring.Small   
import Semiring.View
import Explain.Decomposed
import Explain.Dominators

import Data.List (sortBy)

type Prob  = Double
type State a = a  
type Observed b = b
type Path a b   = View a [State b]

type TransProbTable a = [((State a,State a),Prob)]
type EmisProbTable a b  = [((State a,Observed b),Prob)]


class (Ord a,Ord b,Semiring r) => Viterbi a b r where
  result :: (a,b,VProb) -> r

  viterbi :: (TransProbTable a,EmisProbTable a b) -> [Observed b] -> DP (Int,State a) r 
  viterbi (tps,eps) op (i,state) = 
      case length op == i of
        True  -> (inj.result) (state,op!!0,one) 
        False -> sconcat [memo(i+1,s2) <.> (inj.result) (s2,op!!0,VProb tp1) <.> 
                          (inj.result.lookup' (s2,op!! i) state) eps
                          |((s1,s2),tp1) <- tps, s1 == state] 

  viterbiPath :: (TransProbTable a,EmisProbTable a b) -> [Observed b] -> State a -> r
  viterbiPath p@(tt,et) op s = runDP (viterbi p op) (0,s)


lookup' :: (Ord a,Eq b) => (State a,Observed b) ->  State a -> EmisProbTable a b -> (a,b,VProb)  
lookup' x@(p,q) s l  = 
  case lookup x l of
    Nothing -> (s,q,VProb 0)
    Just v  -> (s,q,VProb v) 


transmission :: TransProbTable Char 
transmission = [(('S','H'),0.6),
                (('H','H' ),0.7), 
                (('H','F'),0.3),
                (('F','F'),0.6), 
                (('F','H' ),0.4),
                (('S','F'),0.4)   
               ]

emission :: EmisProbTable Char Char
emission =  [   (('H','D'),0.1),
                (('H','C'),0.4), 
                (('H','N'),0.5),
                (('F','D'),0.6), 
                (('F','C'),0.3),
                (('F','N'),0.1)   
            ]



transmission1 :: TransProbTable Char 
transmission1 =  [(('S','H'), 0.5),
                  (('H','H'), 0.5), 
                  (('H','L'), 0.5),
                  (('L','L'), 0.6), 
                  (('L','H'), 0.4),
                  (('S','L'), 0.5)   
                 ]


emission1 :: EmisProbTable Char Char
emission1 =  [   
                (('H','A'), 0.2),
                (('H','C'), 0.3), 
                (('H','G'), 0.3),
                (('H','T'), 0.2), 
                (('L','A'), 0.3),
                (('L','C'), 0.2), 
                (('L','G'), 0.2),
                (('L','T'), 0.3)
               ]

instance Viterbi Char Char VProb where
  result (_,_,l) = l

sp1 = viterbiPath (transmission1,emission1) "GGCACTGAA" 'S' :: VProb

instance Viterbi Char Char (Path VProb Char) where
  result (e1,e2,l) = View l [e1]

sp2 = (v, getStates vp)
  where
   View v vp = viterbiPath (transmission1,emission1) "GGCACTGAA" 'S' :: Path VProb Char


vit1 = viterbiPath (transmission,emission) "NCD" 'S' :: VProb

vit2 = (v,getStates vp)
  where
   View v vp = viterbiPath (transmission,emission) "NCD" 'S' :: Path VProb Char

getStates :: [a] -> [a]
getStates = (tail.getAlternate.reverse)
  where
    getAlternate []  = []
    getAlternate [x] = [x]
    getAlternate (x:y:xs) = x:getAlternate xs 
    --getAlternate s = error $ show s 

-- LLLLLLLLLLLHHHHHH
