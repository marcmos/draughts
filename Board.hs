{-# LANGUAGE FlexibleInstances #-}
module Board where

import Data.Maybe
import Data.List
import Data.Foldable

import Field
import Figure
import Utils

type BoardPosition = Int

class BoardChunk c where
  pos :: c -> BoardPosition

-- FIXME: we assume, that BoardPosition is already validated,
-- but it is not implemented... for BoardChunk so too
data BoardField = BoardField BoardPosition Field deriving (Eq, Show)
data BoardFigure = BoardFigure BoardPosition Figure deriving (Eq, Show)

figure :: BoardFigure -> Figure
figure (BoardFigure _ fig) = fig

figureType :: BoardFigure -> FigureType
figureType (BoardFigure _ (Figure _ t)) = t

figureColor :: BoardFigure -> FigureColor
figureColor (BoardFigure _ (Figure c _)) = c

instance BoardChunk BoardField where
  pos (BoardField p _) = p

instance BoardChunk BoardFigure where
  pos (BoardFigure p _) = p

fromBoardField :: BoardField -> Maybe BoardFigure
fromBoardField (BoardField p (Field maybeFigure)) = do
  f <- maybeFigure
  return $ BoardFigure p f

class (Functor a, Foldable a) => Board a where
  size :: a BoardField -> Int
  getField :: a BoardField -> BoardPosition -> Maybe BoardField
  getFigure :: a BoardField -> BoardPosition -> Maybe BoardFigure
  setFigure :: a BoardField -> BoardPosition -> Maybe Figure ->
               Maybe (a BoardField)
  readBoard :: String -> a BoardField
  showLines :: a BoardField -> [String]
  moveFigure :: a BoardField -> BoardPosition -> BoardPosition ->
                Maybe (a BoardField)

removeFigure :: Board a => a BoardField -> BoardPosition -> Maybe (a BoardField)
removeFigure = (\board pos -> setFigure board pos Nothing)

showBoard :: Board a => a BoardField -> String
showBoard board = (unlines . showLines) board

getFields :: Board a => a BoardField -> [BoardPosition] -> [BoardField]
getFields board posList =
  mapMaybe (getField board) posList

filterOccupied :: Board a => a BoardField -> [BoardFigure]
filterOccupied =
  (mapMaybe fromBoardField) . toList

-- -----------------------------------------------------------------------------
-- Board list implementation
instance Board [] where
  size = floor . sqrt . fromIntegral . (* 2) . length

  getField board p =
    (!!) board <$> remapPos board p

  getFigure board pos = do
    (BoardField _ field) <- getField board pos
    fig <- fieldFigure field
    return $ BoardFigure pos fig

  setFigure board pos figure =
    replaceNth board (BoardField pos (Field figure)) <$> remapPos board pos

  readBoard s = concat $ zipWith (readBoardLine size) [0..] rows
    where rows = lines s
          size = length rows

  showLines board =
    let rawChars = splitEvery (size board `quot` 2) $ map showBoardField board
        padChar = showField (Field Nothing)
        delimitedChars = map (intersperse padChar) rawChars
    in mapOddEven (append padChar) (prepend padChar) delimitedChars

  moveFigure board initPos finalPos = do
    (BoardFigure _ fig) <- getFigure board initPos
    board <- setFigure board finalPos (Just fig)
    removeFigure board initPos

showBoardField :: BoardField -> Char
showBoardField (BoardField _ f) = showField f

remapPos :: [BoardField] -> BoardPosition -> Maybe Int
remapPos b p =
  if mappedPos < 0 || mappedPos >= (size b) ^ 2 then Nothing else Just mappedPos
  where mappedPos = p - 1

readBoardLine :: Int -> Int -> String -> [BoardField]
readBoardLine bsize line str =
  zipWith constructField [0..] fields
  where
    calcPos p = (bsize `quot` 2) * line + p + 1
    constructField offset field = BoardField (calcPos offset) field
    fields = map readField $ every (if odd line then odd else even) str
