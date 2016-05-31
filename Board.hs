{-# LANGUAGE FlexibleInstances #-}
module Board
where

import Data.Maybe

import Field
import Figure
import Utils

type BoardColumn = Int
type BoardRow = Int
type BoardPosition = (BoardRow, BoardColumn)

class BoardChunk c where
  pos :: c -> BoardPosition

data BoardField = BoardField BoardPosition Field deriving Show
data BoardFigure = BoardFigure BoardPosition Figure deriving Show

figure :: BoardFigure -> Figure
figure (BoardFigure _ fig) = fig

instance BoardChunk BoardField where
  pos (BoardField p _) = p

instance BoardChunk BoardFigure where
  pos (BoardFigure p _) = p

fromBoardField :: BoardField -> Maybe BoardFigure
fromBoardField (BoardField p (Field maybeFigure)) = do
  f <- maybeFigure
  return $ BoardFigure p f

class Board a where
  size :: a -> Int
  getField :: a -> BoardPosition -> Maybe BoardField
  getFigure :: a -> BoardPosition -> Maybe BoardFigure
  setFigure :: a -> BoardPosition -> Maybe Figure -> Maybe a
  readBoard :: String -> a
  showLines :: a -> [String]
  moveFigure :: a -> BoardPosition -> BoardPosition -> Maybe a
  remapPos :: a -> BoardPosition -> Maybe BoardPosition

isPosInBoard :: Board a => a -> BoardPosition -> Bool
isPosInBoard board (row, col) =
    row > 0 && row <= (size board) &&
    col > 0 && col <= (size board) &&
    odd row /= odd col

removeFigure :: Board a => a -> BoardPosition -> Maybe a
removeFigure = (\board pos -> setFigure board pos Nothing)

showBoard :: Board a => a -> String
showBoard board = (unlines . showLines) board

getDistance :: BoardPosition -> BoardPosition -> Maybe Int
getDistance (fstRow, fstCol) (sndRow, sndCol) =
  let rowDist = abs $ fstRow - sndRow
      colDist = abs $ fstCol - sndCol
  in if rowDist == colDist then Just rowDist else Nothing

getFields :: Board a => a -> [BoardPosition] -> [BoardField]
getFields board posList =
  mapMaybe (getField board) posList

filterOccupied :: [BoardField] -> [BoardFigure]
filterOccupied =
  mapMaybe fromBoardField

instance Board [[Field]] where
  size a = length a

  getField board pos =
    (\(row, col) -> BoardField pos (board !! row !! col)) <$> remapPos board pos

  getFigure board pos = do
    (BoardField _ field) <- getField board pos
    fig <- fieldFigure field
    return $ BoardFigure pos fig

  setFigure board pos figure = do
    (row, col) <- remapPos board pos
    return (replaceNth row board (replaceNth col (board !! row) (Field figure)))

  readBoard s = mapOddEven (readBoardLine odd) (readBoardLine even) (lines s)

  showLines board =
    let padChar = '.'
    in mapOddEven
       (showBoardLine (append padChar))
       (showBoardLine (prepend padChar)) board

  moveFigure board initPos finalPos = do
    (BoardFigure _ fig) <- getFigure board initPos
    board1 <- setFigure board finalPos (Just fig)
    board2 <- removeFigure board1 initPos
    return board2

  remapPos board (row, col) =
    let mappedRow = row - 1
        mappedCol = if odd row then col `quot` 2 - 1 else col `quot` 2
        mappedPos = (mappedRow, mappedCol)
    in if isPosInBoard board (row, col)
       then Just mappedPos
       else Nothing


readBoardLine :: (Int -> Bool) -> String -> [Field]
readBoardLine parity str =
  map readField (every parity str)

showBoardLine :: (Char -> String) -> [Field] -> String
showBoardLine pad line =
  concat (map (pad . showField) line)

-- BoardPosition generator
-- genPos rows = [(c, d) | c <- [1..8], d <- rows, or [and [even c, even d],
--                                                     and [odd c, odd d]]]
