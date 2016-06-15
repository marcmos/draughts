-- Neighborhood generation
module Neighborhood where

import Data.Foldable (toList)
import Data.List (unfoldr)

import Board
import Figure
import BoardChunk

-- Diagonals
data Diagonals t = Diagonals {
  left :: t,
  right :: t
  } deriving Show

instance Functor Diagonals where
  fmap f diag =
    Diagonals {left = f (left diag),
               right = f (right diag)}

instance Foldable Diagonals where
  foldr f z t = f (left t) (f (right t) z)

-- Neighborhood -- forward and backward diagonals

-- FIXME: this assumes, that origin is BoardFigure, but generally it is a Chunk
data Neighborhood t = Neighborhood {
  origin :: BoardFigure,
  forward :: Diagonals t,
  backward :: Diagonals t
  } deriving Show

instance Functor Neighborhood where
  fmap f n =
    Neighborhood {origin = origin n,
                  forward = fmap f $ forward n,
                  backward = fmap f $ backward n}

instance Foldable Neighborhood where
  foldr f z t =
    let lf = (left . forward) t
        rf = (right . forward) t
        lb = (left . backward) t
        rb = (right . backward) t
    in f lf (f rf (f lb (f rb z)))

-- -----------------------------------------------------------------------------
-- Neighborhood

neighborPos :: Int -> Int -> (Int -> Int -> Int) -> (Int -> Int -> Int) ->
               Maybe Int
neighborPos base p rowf colf = do
  (row, col) <- to2D base p
  toPos base ((row `rowf` 1), (col `colf` 1))

forwardLeft :: Int -> Int -> Maybe Int
forwardLeft base p = neighborPos base p (-) (-)

forwardRight :: Int -> Int -> Maybe Int
forwardRight base p = neighborPos base p (-) (+)

backwardLeft :: Int -> Int -> Maybe Int
backwardLeft base p = neighborPos base p (+) (-)

backwardRight :: Int -> Int -> Maybe Int
backwardRight base p = neighborPos base p (+) (+)

unfoldrDir :: Int -> Int -> (Int -> Int -> Maybe Int) -> [Int]
unfoldrDir base p posf =
  let nextSeed = fmap (\x -> (x, x))
  in unfoldr (nextSeed . (posf base)) p

forwardPositions :: Int -> Int -> Diagonals [Int]
forwardPositions base p =
  Diagonals (unfoldrDir base p forwardLeft) (unfoldrDir base p forwardRight)

backwardPositions :: Int -> Int -> Diagonals [Int]
backwardPositions base p =
  Diagonals (unfoldrDir base p backwardLeft) (unfoldrDir base p backwardRight)

neighborhood :: Board t => t BoardField -> BoardFigure -> Int ->
                Neighborhood [BoardField]
neighborhood b bf radius =
  let p = pos bf
      toFields = fmap $ getFields b
      limit = take radius
  in Neighborhood {origin = bf,
                   forward = toFields $ limit <$> forwardPositions (size b) p,
                   backward = toFields $ limit <$> backwardPositions (size b) p}

orientNeighborhood :: Neighborhood a -> Neighborhood a
orientNeighborhood (Neighborhood (BoardFigure p (Figure Black fg)) fw bw) =
  Neighborhood (BoardFigure p (Figure Black fg)) bw fw
orientNeighborhood n = n

flattenNbrhd :: Neighborhood [a] -> [a]
flattenNbrhd = (concat . toList)
