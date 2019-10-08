module Semiring.Pair where 
  
import Semiring.Semiring
instance (Semiring a,Semiring b) => Semiring (a,b) where
    zero = (zero,zero)
    one  = (one,one) 
    (x,y) <+> (z,w) = (x <+> z,y <+> w)
    (x,y) <.> (z,w) = (x <.> z,y <.> w)

