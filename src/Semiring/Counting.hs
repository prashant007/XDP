{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Semiring.Counting where
import Semiring.Semiring

-- | The 'Counting' semiring keeps track of the number of paths 
--   or derivations led to a given output.

instance Semiring Integer where
  {zero = 0; one = 1; (<+>) = (+); (<.>) = (*)}

--instance WeightedSemiring Counting 