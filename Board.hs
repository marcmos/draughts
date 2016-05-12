module Board where
import Field
import Utils

type BoardLine = [Field]
type Board = [BoardLine]

type BoardColumn = Int          -- TODO: own Ord type?
type BoardRow = Int
type BoardPosition = (BoardColumn, BoardRow)

getBoardSize board = length board

getBottomLine = "  12345678"

showBoardLine pad line =
  concat (map (pad . showField) line)

showBoard pad board =
  let rows = mapOddEven
        (showBoardLine (append pad))
        (showBoardLine (prepend pad)) board
  in unlines ((addAnnotations rows) ++ [getBottomLine])

readBoardLine parity str =
  map readField (every parity str)

readBoard str =
  mapOddEven (readBoardLine odd) (readBoardLine even) (lines str)

generateRowAnnotations =
  [show num | num <- [1 ..]]

addAnnotations board =
  let countedRows = zip generateRowAnnotations board
  in map (\x -> (fst x) ++ " " ++ (snd x)) countedRows

isInBoard size (col, row) =
  col >= 0 && row >= 0 && col < size && row < size

-- UNUSED
-- genPos rows = [(c, d) | c <- [1..8], d <- rows, or [and [even c, even d],
--                                                     and [odd c, odd d]]]

replaceRowWith line pos figure =
  replaceNth line pos (Field figure)

boardApplyAt board (col, row) f =
  replaceNth board row (f (board !! row) col)

removeRowFigure line pos =
  replaceRowWith line pos Nothing
removeFigure board (col, row) =
  boardApplyAt board (col, row) removeRowFigure

replaceRowFigure line pos figure =
  replaceRowWith line pos (Just figure)
replaceFigure board (col, row) figure =
  boardApplyAt board (col, row) (\line pos -> replaceRowFigure line pos figure)

getField board (col, row) =
  board !! row !! col

-- filtering
filterInBoard size moves =
  filter (isInBoard size) moves

filterOccupied board posList =
  filter (\x -> getField board x /= Field Nothing) posList
