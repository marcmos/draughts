module Move where

import Data.Maybe

import Board
import Field
import Figure
import BoardChunk
import Neighborhood
import Step
import Capture

--type CaptureSeq = [Capture]
--data Move t = Either [CaptureSeq] Step

-- moves :: Board t => t BoardField -> BoardFigure -> Maybe Move
-- moves b bf
--   | null capt == False = pure $ Left $ capt
--   | isJust steps       = pure $ Right $ fromJust steps
--   | otherwise          = Nothing
--   where nbrs = neighborhood b bf (figureRange b (figureType bf))
--         --capt = (catMaybes . toList) $ capture (size b) bf <$> nbrs
--         capt = []
--         steps = step b nbrs

-- replacePos :: FigureType -> BoardField -> [BoardPosition] -> BoardField
-- replacePos _ (BoardField p (Field Nothing)) _ = BoardField p (Field Nothing)
-- replacePos ft bf posL =
--   if elem p posL then BoardField p (Field $ Just $ Figure c ft) else bf
--   where (BoardField p (Field (Just (Figure c _)))) = bf

-- transformKings :: Board t => t BoardField -> t BoardField
-- transformKings b =
--   transform <$> b
--   where
--     transform bf = case bf of
--       BoardField _ (Field (Just (Figure White Pawn))) ->
--         replacePos King bf whiteFields
--       BoardField _ (Field (Just (Figure Black Pawn))) ->
--         replacePos King bf blackFields
--       _ -> bf
--     rowFields = size b `quot` 2
--     whiteFields = [1..rowFields]
--     blackFields = [((size b - 1) * rowFields + 1)..(size b * rowFields)]

-- evalMove :: Board t => t BoardField -> Move -> [(BoardFigure, t BoardField)]
-- evalMove b m = []
  -- either (evalCaptures b) (evalStep b) m
