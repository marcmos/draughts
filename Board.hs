{-# LANGUAGE FlexibleInstances #-}
module Board (BoardPosition, Board (..), isPosInBoard, removeFigure,
              showBoard) where
import Field
import Figure
import Utils

type BoardColumn = Int
type BoardRow = Int
type BoardPosition = (BoardRow, BoardColumn)

class Board a where
  size :: a -> Int
  getField :: a -> BoardPosition -> Maybe Field
  setFigure :: a -> BoardPosition -> Maybe Figure -> Maybe a
  readBoard :: String -> a
  showLines :: a -> [String]
  moveFigure :: a -> BoardPosition -> BoardPosition -> Maybe a

isPosInBoard :: Board a => a -> BoardPosition -> Bool
isPosInBoard board (row, col) =
    row > 0 && row <= (size board) &&
    col > 0 && col <= (size board) &&
    odd row /= odd col

removeFigure :: Board a => a -> BoardPosition -> Maybe a
removeFigure = (\board pos -> setFigure board pos Nothing)

showBoard :: Board a => a -> String
showBoard board = (unlines . showLines) board

instance Board [[Field]] where
  size a = length a

  getField board pos = do
    (col, row) <- remapPos board pos
    return (board !! col !! row)

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
    Field figure <- getField board initPos
    board1 <- setFigure board finalPos figure
    board2 <- removeFigure board1 initPos
    return board2

readBoardLine :: (Int -> Bool) -> String -> [Field]
readBoardLine parity str =
  map readField (every parity str)

showBoardLine :: (Char -> String) -> [Field] -> String
showBoardLine pad line =
  concat (map (pad . showField) line)

remapPos :: [[Field]] -> BoardPosition -> Maybe BoardPosition
remapPos board (row, col) =
  let mappedRow = row - 1
      mappedCol = if odd row then col `quot` 2 - 1 else col `quot` 2
      mappedPos = (mappedRow, mappedCol)
  in if isPosInBoard board (row, col)
     then Just mappedPos
     else Nothing

-- BoardPosition generator
-- genPos rows = [(c, d) | c <- [1..8], d <- rows, or [and [even c, even d],
--                                                     and [odd c, odd d]]]
