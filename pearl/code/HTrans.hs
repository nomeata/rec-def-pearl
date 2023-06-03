module HTrans where

import qualified Data.Map as M
import qualified Data.Set as S

import Hatafun

type Graph = M.Map Int [Int]

rTrans :: Graph -> Graph
rTrans g = M.map S.toList reaches
  where
    reaches :: M.Map Int (S.Set Int)
    reaches = M.mapWithKey (\v vs -> S.insert v (S.unions [ reaches M.! v' | v' <- vs ])) g

example1 :: Graph
example1 = M.fromList [(1,[3]),(2,[1,3]),(3,[])]

example2 :: Graph
example2 = M.fromList [(1,[2,3]),(2,[1,3]),(3,[])]
