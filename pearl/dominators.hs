import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Recursive.Set as RS

example :: [(Int, Int)]
example = [(1,2),(1,3),(2,4),(3,4),(4,3)]

intersections :: [S.Set Int] -> S.Set Int
intersections [] = S.empty
intersections xs = foldl1 S.intersection xs

preds :: [(Int,Int)] -> M.Map Int [Int]
preds edges = M.fromListWith (<>) $
    [ (v1, [])   | (v1, _) <- edges ] ++ -- to make the map total
    [ (v2, [v1]) | (v1, v2) <- edges ]

domintors1 :: [(Int,Int)] -> M.Map Int [Int]
domintors1 edges = fmap S.toList doms
  where
    doms :: M.Map Int (S.Set Int)
    doms = M.mapWithKey
        (\v vs -> S.insert v (intersections [ doms M.! v' | v' <- vs]))
        (preds edges)

intersections' :: [RS.RSet Int] -> RS.RSet Int
intersections' [] = RS.empty
intersections' xs = foldl1 RS.intersection xs

domintors2 :: [(Int,Int)] -> M.Map Int [Int]
domintors2 edges = fmap (S.toList . RS.get) doms
  where
    doms :: M.Map Int (RS.RSet Int)
    doms = M.mapWithKey
        (\v vs -> RS.insert v (intersections' [ doms M.! v' | v' <- vs]))
        (preds edges)

unions' :: S.Set Int -> [RS.RSet Int] -> RS.RSet Int
unions' univ [] = RS.mk univ
unions' _ xs = RS.unions xs


domintors3 :: [(Int,Int)] -> M.Map Int [Int]
domintors3 edges = fmap (S.toList . S.difference nodes . RS.get) nonDoms
  where
    nodes = S.fromList [v | (v1,v2) <- edges, v <- [v1,v2]]
    nonDoms :: M.Map Int (RS.RSet Int)
    nonDoms = M.mapWithKey
        (\v vs -> RS.delete v (unions' nodes [ nonDoms M.! v' | v' <- vs]))
        (preds edges)

