\iffalse

> module HTrans where
> 
> import qualified Data.Map as M
> import qualified Data.Set as S
> 
> import Hatafun hiding (Graph)

\fi

> type Graph = M.Map Int (S.Set Int)
> 
> rTrans :: Graph -> Graph
> rTrans = eval $ lam $ \edge_graph ->
>   let  reaches = mlam $ \graph ->
>          edge_graph `lub` mapWithKey (
>            lam $ \k -> lam $ \vs -> insert `app` k `mapp` mbind vs (lam $ \v' -> graph ! v')
>          ) graph
>   in fix reaches

\iffalse

> example1 :: Graph
> example1 = M.map S.fromList $ M.fromList [(1,[3]),(2,[1,3]),(3,[])]
> 
> example2 :: Graph
> example2 = M.map S.fromList $ M.fromList [(1,[2,3]),(2,[1,3]),(3,[])]

\fi
