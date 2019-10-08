{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveAnyClass #-}

module Examples.HMM where

-- Haskell imports
import Data.List (nub,sortBy)
import Data.Maybe (fromJust,fromMaybe)
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

type Path a b = View a [b]

type TTable a c = [((a,a),c)]
type ETable a b c = [((a,b),c)]




instance Decompose (Small (Decomposed Double)) Double where
  dec NegInfinity  = Values []
  dec (Finite' vs) = vs
  supportive _ x = x<0

class (Ord a,Ord b,Num c,Ord c,Semiring r) => Viterbi a b c r where
  result :: (a,b,Small c) -> r

  viterbi :: (TTable a c,ETable a b c) -> [b] -> DP (Int,a) r
  viterbi (ts,es) xs (i,k)
     | length xs == i = res (k,xs!!0,one)
     | otherwise      =
       sconcat [memo(i+1,j) <.> res (k,xs!!0,Finite' p) <.>
                (res.lookupE (j,xs!!i) k) es | ((s,j),p) <- ts, s==k]
     where res = inj . result

  viterbiPath :: (TTable a c ,ETable a b c) -> [b] -> a -> r
  viterbiPath tes xs s = runDP (viterbi tes xs) (0,s)


lookupE :: (Ord a,Eq b) => (a,b) -> a -> ETable a b c -> (a,b,Small c)
lookupE x@(p,q) s l = maybe (s,q,NegInfinity) (\v->(s,q,Finite' v)) (lookup x l) 


transition :: TTable Char Double
transition = [
              (('A','G'), 0.4),
              (('A','V'), 0.3),
              (('A','B'), 0.3),
              (('G','G'), 0.6),
              (('G','V'), 0.25),
              (('G','B'), 0.15),
              (('V','G'), 0.2),
              (('V','V'), 0.6),
              (('V','B'), 0.2),
              (('B','G'), 0.2),
              (('B','V'), 0.3),
              (('B','B'), 0.5)]


transitionD :: TTable Char [Double]
transitionD = [
              (('A','G'), [0.694,0.8,0.8,0.9]),--0.4),
              (('A','V'), [0.7,0.893,0.6,0.8]), --0.3),
              (('A','B'), [0.8,0.6,0.7,0.893]),-- 0.3),
              (('G','G'), [0.9,0.8,0.9,0.926]), -- 0.6),
              (('G','V'), [0.7,0.8,0.8,0.558]), -- 0.25),
              (('G','B'), [0.6,0.6,0.7,0.595]), --0.15),
              (('V','G'), [0.8,0.8,0.39,0.8]), --0.2),
              (('V','V'), [0.9,0.926,0.8,0.9]), --0.6),
              (('V','B'), [0.8,0.8,0.8,0.39]), --0.2),
              (('B','G'), [0.39,0.8,0.8,0.8]),  --0.2),
              (('B','V'), [0.7,0.893,0.8,0.6]),
              (('B','B'), [0.868,0.8,0.8,0.9])] --0.5)]



emission :: ETable Char Char Double
emission =   [(('G','S'), 0.8),
              (('G','R'), 0.1),
              (('G','C'), 0.05),
              (('G','I'), 0.05),
              (('V','S'), 0.6),
              (('V','R'), 0.1),
              (('V','C'), 0.1),
              (('V','I'), 0.2),
              (('B','S'), 0.1),
              (('B','R'), 0.4),
              (('B','C'), 0.3),
              (('B','I'), 0.2)]


instance Viterbi Char Char Double (Small Double) where
  result (_,_,l) = l


sp1 = viterbiPath (t1,e1) "RSS" 'A' :: Small Double
  where
      f  = map (\(x,y) -> (x,log y))
      t1 = f transition 
      e1 = f emission 


getStates :: [a] -> [a]
getStates = (tail.getAlternate.reverse)
  where
    getAlternate []  = []
    getAlternate [x] = [x]
    getAlternate (x:y:xs) = x:getAlternate xs


instance Viterbi Char Char Double (Path (Small Double) Char) where
  result (e,_,l) = View l [e]

sp2 = (v,getStates vp)
  where
    View v vp = viterbiPath (t1,e1) "RSS" 'A' :: Path (Small Double) Char
    f  = map (\(x,y) -> (x,log y))
    t1 = f transition 
    e1 = f emission


exp' :: Small Double -> Double
exp' NegInfinity  = 0
exp' (Finite' i) = exp i 


instance Viterbi Char Char (Decomposed Double) DecomPath where
  result (e,_,l) = View l [e]


type SmallDecomp = Small(Decomposed Double)
type DecomPath   = Path SmallDecomp Char

sp3 = (v,getStates vp)
  where
    View v vp = viterbiPath (f1 transitionD,f2 emission) "RSS" 'A' :: DecomPath
    f1 = map (\(x,y) -> (x,g y))
    f2 = map (\(x,y) -> (x,g [y]))
    g = Values . map log 

    getStates :: [a] -> [a]
    getStates = (tail.getAlternate.reverse)
    getAlternate []  = []
    getAlternate [x] = [x]
    getAlternate (x:y:xs) = x:getAlternate xs


hFun NegInfinity = NegInfinity 
hFun (Finite' vs) = finVal ls 
   where
    Values ls = vs 


--(Finite' (Values {values = [-3.32487654541345,-0.6694306539426291,-0.43386458262986227,-0.25912260432974155]}),"GGG")

finVal = Finite' . Values 

sp3' = finVal [-2.92487654541345,-0.16,-0.143386458262986227,-0.25912260432974155]

categories = ["Arctic Winds","Ocean Currents","High Altitude","Snowy"]

domKS = explainWith categories (fst sp3) sp3' 

