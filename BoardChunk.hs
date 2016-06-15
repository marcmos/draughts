module BoardChunk where

import Data.Maybe
import Data.List (sortOn)

import Board

-- Coordinate calculation
validate2D :: Int -> (Int, Int) -> Maybe (Int, Int)
validate2D base (col, row) =
  if col >= 1 && col <= base &&
     row >= 1 && row <= base &&
     odd col /= odd row
  then Just (col, row)
  else Nothing

to2D :: Int -> Int -> Maybe (Int, Int)
to2D base p
  | odd row   = validate2D base (row + 1, col * 2 + 1)
  | otherwise = validate2D base (row + 1, col * 2 + 2)
  where (row, col) = divMod (p - 1) (base `quot` 2)

toPos :: Int -> (Int, Int) -> Maybe Int
toPos base (row, col)
  | row < 1 || row > base   = Nothing
  | col < 1 || col > base   = Nothing
  | odd row == odd col      = Nothing
  | otherwise               = Just $ p
  where normCol = if odd row then col - 1 else col
        p = (row - 1) * (base `quot` 2) + normCol `quot` 2 + 1

--------------------------------------------------------------------------------
-- Chunk distance calculation

-- TODO: these calculations are parametrized by board size;
--       consider keeping the size of boards in BoardChunk type itself
chunkDistance :: (BoardChunk c1, BoardChunk c2, Board b) => b BoardField ->
                 c1 -> c2 -> Int
chunkDistance b c1 c2 =
  let (fstRow, fstCol) = fromJust $ to2D (size b) $ pos c1
      (sndRow, sndCol) = fromJust $ to2D (size b) $ pos c2
      rowDist = abs $ fstRow - sndRow
      colDist = abs $ fstCol - sndCol
  in if rowDist == colDist
     then rowDist
     else error "Move.chunkDistance: column and row distance are different"

closestChunks :: (BoardChunk c1, BoardChunk c2, Board b) => b BoardField -> c1
                 -> [c2] -> [(c2, Int)]
closestChunks b c l =
  sortOn snd (zip l (map (chunkDistance b c) l))

constrainDist :: (BoardChunk c1, BoardChunk c2, Board b) =>
                 (Int -> Int -> Bool) -> b BoardField -> c1 -> [c2] -> Int ->
                 [c2]
constrainDist f b refPos posList dist =
  filter (\chunk -> (chunkDistance b refPos chunk) `f` dist) posList

constrainMinDist :: (BoardChunk c1, BoardChunk c2, Board b) => b BoardField ->
                    c1 -> [c2] -> Int -> [c2]
constrainMinDist = constrainDist (>)

constrainMaxDist :: (BoardChunk c1, BoardChunk c2, Board b) => b BoardField ->
                    c1 -> [c2] -> Int -> [c2]
constrainMaxDist = constrainDist (<)

filterBeforeChunk :: (BoardChunk c1, BoardChunk c2, BoardChunk c3, Board b) =>
                     b BoardField -> c1 -> [c2] -> c3 -> [c2]
filterBeforeChunk b orig chunks dest =
  constrainMaxDist b orig chunks (chunkDistance b orig dest)

filterAfterChunk :: (BoardChunk c1, BoardChunk c2, BoardChunk c3, Board b) =>
                    b BoardField -> c1 -> [c2] -> c3 -> [c2]
filterAfterChunk b orig chunks dest =
  constrainMinDist b orig chunks (chunkDistance b orig dest)

closestFigures :: (BoardChunk c, Board b) => b BoardField -> c ->
                  [BoardField] -> [BoardFigure]
closestFigures b chunk =
  fst . unzip . (closestChunks b chunk) . (mapMaybe fromBoardField)

closestFigure :: (BoardChunk c, Board b) => b BoardField -> c -> [BoardField] ->
                 Maybe BoardFigure
closestFigure b c bf =
  let figures = closestFigures b c bf
  in if null figures then Nothing else Just $ head figures
