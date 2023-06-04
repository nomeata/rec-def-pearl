> module Cell (Cell, newCell, defCellInsert, getCell) where
>
> import Control.Monad (join, unless)
> import Control.Concurrent.MVar
> import qualified Data.Set as S
>
> data Cell a = MkCell { val :: MVar (S.Set a), onChange :: MVar (IO ()) }
>
> newCell :: IO (Cell a)
> newCell = do
>     m <- newMVar S.empty
>     notify <- newMVar (pure ())
>     pure $ MkCell m notify
>
> getCell :: Cell a -> IO (S.Set a)
> getCell (MkCell m _) = readMVar m
>
> setCell :: Eq a => Cell a -> S.Set a -> IO ()
> setCell (MkCell m notify) x = do
>     old <- swapMVar m x
>     unless (old == x) $ join (readMVar notify)
>
> watchCell :: Cell a -> IO () -> IO ()
> watchCell (MkCell m notify) act =
>     modifyMVar_ notify (\a -> pure (act >> a))
>
> defCellInsert :: Ord a => Cell a -> a -> Cell a -> IO ()
> defCellInsert c1 x c2 = do
>     watchCell c2 update
>     update
>   where
>     update = do
>         s <- getCell c2
>         setCell c1 (S.insert x s)
