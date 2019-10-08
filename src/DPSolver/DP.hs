{-
****************************************************************************************************************
This code is primarily Sasha Rush's Pragmatic DP Haskell library (http://hackage.haskell.org/package/DP-0.1.1/).
The DP.hs file make some changes from Sasha Rush's original file of the same name in the aforementioned 
library. These changes are:

1) Functions memo and inj defined here are not part of the original code. 
2) The DPGen type synonym is called DP in the orifinal file. This type change propagates to
   the fromSimple function which is of DPGen now instead of DP. We made this change as we wanted
   to use DP type synonym elsewhere.

-- | A dynamic program. 
type DPGen index cell = index -> DPCell index ce
-- | Convert a Simple DP to a General DP 

fromSimple :: SimpleDP a b -> DPGen a (Identity b)
fromSimple simple i = mkCell [mkItem () (simple i)] 
****************************************************************************************************************
-}
module DPSolver.DP where

import DPSolver.Internals
import qualified Data.Map as M 
import Control.Monad.Identity

type SimpleDP ind val = ind -> DPSubValue ind (Identity val)


-- | Retrieve the solution of subproblem of the DP for a given index.
--   Note: The ordering and storage of this retrieval is determined by the
--   solver used. 
f :: ind -> DPSubValue ind (Identity val)
f =  DPNode ()


memo :: ind -> DPSubValue ind (Identity val)
memo =  DPNode ()

-- | Lift a semiring value into a DPSubValue. 
inj :: CellVal cell -> DPSubValue index cell
inj = Constant


-- | A dynamic program. 
type DPGen index cell = index -> DPCell index cell 

-- | Groups several items into a cell. Items with the same key are combined with @mappend@
mkCell :: [DPItem index cell] -> DPCell index cell
mkCell = Many

-- | Create a new item. (@'mkItem' key val@) will create an item subindexed by key with value val.mkItem :: CellKey cell -> DPSubValue index cell -> DPItem index cell
mkItem = DPItem 

-- | Lookup a cell from the chart. @'getCell' ind fn@ will lookup index @ind@ and then call @fn@  
--   repeatedly with each item in the cell. It then concats the resulting items, combining 
--   similarly keyed items with @mappend@
--   Use instead of @f@ for Chart-Cell DPs 
getCell
  :: index
     -> (Item index cell -> DPCell index cell)
     -> DPCell index cell
getCell = Request

-- | Convert a Simple DP to a General DP 
fromSimple :: SimpleDP a b -> DPGen a (Identity b)
fromSimple simple i = mkCell [mkItem () (simple i)] 
