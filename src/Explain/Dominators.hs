{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Explain.Dominators where


import Data.Set (powerSet,fromList,toList)
import Data.List (sortBy)
import Data.Function (on)

import Semiring.Semiring

import Explain.Decomposed
import Explain.Labeled 
import Semiring.Large -- remove later


-- partition is contained in some old Prelude version
--
partition :: (a -> Bool) -> [a] -> ([a],[a])
partition p xs = (filter p xs, filter (not.p) xs)

sublists :: Ord a => [a] -> [[a]]
sublists = map toList.filter (not.null).toList.powerSet.fromList


-- Connecting semirings with decompositions
--
class Semiring a => Decompose a b | a -> b where
  dec :: a -> Decomposed b
  -- lift :: Decomposed b -> a
  supportive :: a -> b -> Bool
  -- supportive _ x = y <+> one == y where y = lift $ Values [x]


-- Minimal dominating set
--
mds :: (Decompose a b,Ord b,Num b) => a -> a -> Decomposed b
mds d d' = Values $ head $ sortBy (compare `on` length) doms
    where (support,barrier) = partition (supportive d) $ values (dec d-dec d')
          doms = [d | d <- sublists support, abs (sum d) > abs (sum barrier)]

withCategories :: Decomposed a -> [String] -> Decomposed (Labeled a)
withCategories d cs = Values (map Label cs) <*> d

explainWith :: (Decompose a b,Ord b,Num b) =>
               [String] -> a -> a -> Decomposed (Labeled b)
explainWith cs d d' = Values $ head $ sortBy (compare `on` length) doms
    where (support,barrier) = partition sup $ values delta
          doms = [d | d <- sublists support, abs (sum d) > abs (sum barrier)]
          delta = dec d `withCategories` cs - dec d' `withCategories` cs
          sup = supportive d . unlabel


