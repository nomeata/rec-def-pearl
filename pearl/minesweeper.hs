module Minesweeper where

import Data.Array
import qualified Data.Recursive.Bool as RB

type C = (Int,Int)

type Grid a = Array C a

type Board = Grid Bool
type Mask = Grid Bool

size :: (C,C)
size = ((0,0), (3,3))

board1 :: Board
board1 = listArray size
  [ False, False, False, False
  , True,  False, False, False
  , True,  False, False, False
  , False,  False, False, False
  ]

mask1 :: Mask
mask1 = listArray size
  [ True,  True,  True,  False
  , False, False, False, False
  , False, False, False, False
  , False, False, False, False
  ]

mask2 :: Mask
mask2 = listArray size
  [ True,  True,  False, False
  , False, False, False, False
  , False, False, False, False
  , False, False, False, True
  ]

pGrid :: (C -> String) -> String
pGrid p = unlines [ concat [ p' (y,x) | x <- [lx-1 .. ux+1] ] | y  <- [ly-1 .. uy+1] ]
  where
    ((lx,ly),(ux,uy)) = size

    p' c | inRange size c = p c
    p'  _ = "#"

pBombs :: Board -> String
pBombs b = pGrid $ \c -> if b ! c then "*" else " "

neighbors :: C -> [C]
neighbors (x,y) =
    [ c
    | (dx, dy) <- range ((-1,-1), (1,1))
    , (dx, dy) /= (0,0)
    , let c = (x + dx, y + dy)
    , inRange size c
    ]

data H = Bomb | Hint Int deriving Eq

hint :: Board -> C -> H
hint b c | b ! c = Bomb
hint b c = Hint $ sum [ 1 | c' <- neighbors c, b ! c' ]

pCell :: Board -> C -> String
pCell b c = case hint b c of
    Bomb -> "*"
    Hint 0 -> " "
    Hint n -> show n

pBoard :: Board -> String
pBoard b = pGrid (pCell b)

pMasked :: Board -> Mask -> String
pMasked b m = pGrid $ \c -> if m ! c then pCell b c else "?"

solve0 :: Board -> Mask -> Mask
solve0 b m0 = m1
  where
    m1 :: Mask
    m1 = genArray size $ \c ->
      m0 ! c || or [ m0 ! c' | c' <- neighbors c, hint b c' == Hint 0 ]

solve1 :: Board -> Mask -> Mask
solve1 b m0 = m1
  where
    m1 :: Mask
    m1 = genArray size $ \c ->
      m0 ! c || or [ m1 ! c' | c' <- neighbors c, hint b c' == Hint 0 ]

solve2 :: Board -> Mask -> Mask
solve2 b m0 = fmap RB.get m1
  where
    m1 :: Grid RB.RBool
    m1 = genArray size $ \c ->
      RB.mk (m0 ! c) RB.|| RB.or [ m1 ! c' | c' <- neighbors c, hint b c' == Hint 0 ]

genArray :: Ix i => (i,i) -> (i -> e) -> Array i e
genArray r f = listArray r $ map f $ range r


{-
focusArray :: C -> Array C a -> Array C a
focusArray (dx, dy) (Array (lx,ly) (ux,uy) s a) = (Array (lx-dx,ly-dy) (ux-dx, uy-dy) s a)

convolution :: (Array C a -> a) -> Array C a -> Array C a
convolution f a = genArray (bounds a) $ \c -> f (focusArray c a)

kfix :: (Array C a -> a) -> Array C a
kfix f = a where a = convolution f a
-}
