module Move where

import Board
import Field
import Figure
import Step
import Capture

moves :: Board t => t BoardField -> BoardFigure -> [Path t]
moves b bf
  | null cp   = sp
  | otherwise = cp
  where ctx = FigureCtx bf b
        cp = capturePaths ctx
        sp = stepPaths ctx

replacePos :: FigureType -> BoardField -> [BoardPosition] -> BoardField
replacePos _ (BoardField p (Field Nothing)) _ = BoardField p (Field Nothing)
replacePos ft bf posL =
  if elem p posL then BoardField p (Field $ Just $ Figure c ft) else bf
  where (BoardField p (Field (Just (Figure c _)))) = bf

transformKings :: Board t => t BoardField -> t BoardField
transformKings b =
  transform <$> b
  where
    transform bf = case bf of
      BoardField _ (Field (Just (Figure White Pawn))) ->
        replacePos King bf whiteFields
      BoardField _ (Field (Just (Figure Black Pawn))) ->
        replacePos King bf blackFields
      _ -> bf
    rowFields = size b `quot` 2
    whiteFields = [1..rowFields]
    blackFields = [((size b - 1) * rowFields + 1)..(size b * rowFields)]
