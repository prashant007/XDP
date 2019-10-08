{-
****************************************************************************************************************
This code is primarily Sasha Rush's Pragmatic DP Haskell library (http://hackage.haskell.org/package/DP-0.1.1/).
The Solvers.hs file make some changes from Sasha Rush's original file of the same name in the aforementioned 
library. These changes are:

1) Only a very specific DP solver (used in solving the recurrence relation), top down solver, is used. 
In comparison Sasha Rush's original file exported bottom up solvers as well.

2) runDP function defined here isn't part of the original code of the library. 
****************************************************************************************************************
-}

module DPSolver.Solvers where 

import Control.Monad.Identity
import DPSolver.DP
import DPSolver.TopDown
import DPSolver.SolverAPI
import Semiring.Semiring

type DP a b = SimpleDP a b 

runDP :: (Semiring r, Ord ind) => DP ind r -> ind -> r
runDP x y = getSimpleResult $ runIdentity $ solveSimpleDP topDownMap y x


  