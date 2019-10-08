{-
****************************************************************************************************************
This code is primarily Sasha Rush's Pragmatic DP Haskell library (http://hackage.haskell.org/package/DP-0.1.1/). 
The Internals.hs module make some changes to the original file in the library of the same name. The specific changes 
made to this code are provided as comments here. These are changes owing to the different semiring 
formulation that we have.  

-- ====================================================================================================
-- ====================================================================================================
Our code has 

optFunc Plus = (<+>)
optFunc Times = (<.>)

--- =========================instead of=============================================================== 

optFunc Plus = mappend
optFunc Times = times

-- ====================================================================================================
-- ====================================================================================================
Our code has 

instance (Semiring (CellVal cell)) => Semiring (DPSubValue index cell ) where 
    zero  = Constant zero 
    one   = Constant one   
    (<+>) = Opt Plus
    (<.>) = Opt Times

--- =========================instead of=============================================================== 

instance (Monoid (CellVal cell)) => Monoid (DPSubValue index cell ) where 
    mappend = Opt Plus
    mempty  = Constant mempty

instance (Multiplicative (CellVal cell)) => Multiplicative (DPSubValue index  cell ) where 
    times = Opt Times
    one  = Constant one
instance Semiring (CellVal cell) =>  Semiring (DPSubValue index  cell)

data DPState m chart ind cell = DPState { 
     dpLookup :: ind -> StateT (DPState m chart ind cell) m cell,
     dpData :: chart 
}
****************************************************************************************************************
-}

{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, TypeFamilies, KindSignatures, 
             ExistentialQuantification, ConstraintKinds
#-}
module DPSolver.Internals where
import Control.Monad.State.Strict
import Control.Monad.State.Class
import Control.Monad
import Semiring.Semiring
import qualified Data.Map as M
-- import Control.Applicative 
import Control.Monad.Identity
--import Safe 
import GHC.Stack 
import Data.Maybe 

class (Ord (CellKey c), Semiring (CellVal c)) => Cell (c:: *)  where 
    type CellKey c :: *
    type CellVal  c :: * 
    fromList :: [(CellKey c, CellVal c)] ->  c 
    toList :: c -> [(CellKey c, CellVal c)]  
    cellLookup :: CellKey c -> c -> CellVal c
 
instance (Ord k, Semiring v) => Cell (M.Map k v) where 
     type CellKey (M.Map k v) = k 
     type CellVal (M.Map k v) = v 
    
     fromList  = M.fromListWith (<+>)
     toList = M.toList               
     cellLookup n cells = fromJustNote "Key not found in cell" $ M.lookup n cells

instance (Semiring v) => Cell (Identity v) where 
     type CellKey (Identity v) = () 
     type CellVal (Identity v) = v
     fromList = Identity . snd . head
     toList (Identity v) = [((), v)]
     cellLookup _ (Identity a) = a  
     
data DPOpt = Plus | Times

optFunc Plus = (<+>)
optFunc Times = (<.>)

-- | Explicit representation for items retrieved from a DP
type Item index cell= (CellKey cell, DPSubValue index cell)

data DPCell index cell  = 
    Request index (Item index cell ->  DPCell index cell ) | 
    Many [DPItem index cell]

data DPItem index cell = 
    DPItem (CellKey cell) (DPSubValue index cell)


-- | Represents the solution to a subproblem of the DP.
--   Introduced by @constant@, used through the "Semiring" interface.
data DPSubValue index cell  = 
    DPNode (CellKey cell) index | 
    Constant (CellVal cell) | 
    Opt DPOpt (DPSubValue index cell) (DPSubValue index cell) 

-- -------------------------------------------------------------------
instance (Semiring (CellVal cell)) => Semiring (DPSubValue index cell ) where 
    zero  = Constant zero 
    one   = Constant one   
    (<+>) = Opt Plus
    (<.>) = Opt Times

-- ---------------------------------------------------------------------

data DPState m chart ind cell = DPState { 
     dpLookup :: ind -> StateT (DPState m chart ind cell) m cell,
     dpData :: chart 
}

findCells :: (Monad m, Cell cell) => ind -> StateT (DPState m chart ind cell) m cell
findCells ind = do 
  state <- get
  dpLookup state ind 

reduceBase ::  (Cell cell, Monad m) => 
               (cell -> m cell) ->
               DPCell ind cell -> 
               (ind -> m cell) -> m cell
reduceBase o r fn = reduceBaseWrite o r (lift. fn) ()

reduceBaseWrite ::  (Cell cell,  Monad m) => 
                    (cell -> m cell) -> 
                    DPCell ind cell  -> 
                    (ind -> StateT (DPState m chart ind cell) m cell) -> chart -> m cell
reduceBaseWrite o r fn chart = do 
  res <- evalStateT (reduceComplex $ r) (DPState fn chart)
  o $ fromList res 


reduceComplex :: (Monad m, Cell cell) => DPCell ind cell -> StateT (DPState m chart ind cell) m [(CellKey cell, CellVal cell)] 
reduceComplex (Many a) = do
  rs <- mapM reduceItem a
  return $ rs
reduceComplex (Request ind fn) = do
   cells <- findCells ind
   res <- mapM reduceComplex $ map fn $ map (\(a,b) -> (a, Constant b)) $ toList cells
   return $ concat res

reduceItem :: (Monad m, Cell cell) => DPItem ind cell -> StateT (DPState m chart ind cell) m (CellKey cell, CellVal cell) 
reduceItem (DPItem n a) = do
    ared <- reduce a 
    return $ (n, ared)  

reduce :: (Monad m, Cell cell) => DPSubValue ind cell -> StateT (DPState m chart ind cell) m (CellVal cell) 
reduce (Constant a)  = return a
reduce (Opt opt a b) = (liftM2 $ optFunc opt) (reduce a) (reduce b)
reduce (DPNode n i)   = do
   state <- get
   cell  <- dpLookup state i 
   return $ cellLookup n cell 

-- ===============================================================================
type Partial = HasCallStack

fromJustNote :: Partial => String -> Maybe a -> a
fromJustNote note x = withFrozenCallStack $ fromNote note "fromJustNote Nothing" x

fromNote :: Partial => String -> String -> Maybe a -> a
fromNote = fromNoteModule "Safe"

fromNoteModule :: Partial => String -> String -> String -> Maybe a -> a
fromNoteModule modu note func = fromMaybe (error msg)
    where msg = modu ++ "." ++ func ++ (if null note then "" else ", " ++ note)