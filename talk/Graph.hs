module Graph where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Recursive.Set as RS

type Graph = M.Map Int [Int]

rtrans1 :: Graph -> Graph
rtrans1 g = M.map S.toList reaches
  where
    reaches :: M.Map Int (S.Set Int)
    reaches = M.mapWithKey f g

    f v vs = S.insert v (S.unions [ reaches M.! v' | v' <- vs ])


graph1 :: Graph
graph1 = M.fromList [(1,[3]),(2,[1]),(3,[])]

graph2 :: Graph
graph2 = M.fromList [(1,[2,3]),(2,[1]),(3,[])]

rtrans2 :: Graph -> Graph
rtrans2 g = M.map (S.toList . RS.get) reaches
  where
    reaches :: M.Map Int (RS.RSet Int)
    reaches = M.mapWithKey f g

    f v vs = RS.insert v (RS.unions [ reaches M.! v' | v' <- vs ])
