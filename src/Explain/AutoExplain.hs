{-# LANGUAGE FlexibleContexts #-}

module Explain.AutoExplain where


import Explain.Dominators
import Explain.Decomposed
import Explain.Principal
import Explain.Labeled


explain :: (FromPrincipal f,Decompose (f (Decomposed a)) b,
            Eq (f (Decomposed a)),Ord b,Num b) =>
            (i -> (f (Decomposed a), f (Principal a))) -> [String] -> i ->
            (f (Decomposed a),Maybe (f (Decomposed a),Decomposed (Labeled b)))

explain f cs i | o==o'     = (o,Nothing)
                | otherwise = (o,Just (o',explainWith cs o o'))
                where 
                  o' = fromPrincipal o1 
                  (o,o1) = f i


