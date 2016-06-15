-- Captures
module Capture where

import Data.Tree

import Data.List (intersect)
import Data.Foldable (toList)
import Data.Maybe (catMaybes)

import BoardChunk
import Board
import Neighborhood
import Figure
import Step

data Capture = Capture Step BoardFigure
data CaptureEnv b = CaptureEnv (StepEnv b) BoardFigure deriving Show
data CaptureTree b = CaptureTree (FigureCtx b) (Forest (CaptureEnv b))
  deriving Show

-- Diagonal jumps
diagJump :: Board b => FigureCtx b -> [BoardField] ->
        Maybe (BoardFigure, [BoardPosition])
diagJump (FigureCtx bf b) f =
  if null figures || null landings then Nothing else Just (closest, landings)
  where figures = closestFigures b bf f
        closest = head figures
        after = filterAfterChunk b bf f closest
        beforeNext = filterBeforeChunk b bf f (figures !! 1)
        landings = map pos $ if length figures < 2 || null beforeNext
                             then after else intersect after beforeNext

diagCapture :: Board b => FigureCtx b -> [BoardField] -> Maybe Capture
diagCapture fc fs =
  do
    (jf, ls) <- diagJump fc fs
    _ <- if isAlly (figure bf) (figure jf) then Nothing else Just True
    return $ Capture (Step bf ls) jf
  where (FigureCtx bf _) = fc

-- Capture evaluation
figCaptures :: Board b => FigureCtx b -> [Capture]
figCaptures fc =
  catMaybes . toList $ diagCapture fc <$> nbr
  where (FigureCtx bf b) = fc
        nbr = (neighborhood b bf) . (figureRange b) . figureType $ bf

stepCaptures :: Board b => FigureCtx b -> [CaptureEnv b]
stepCaptures fc =
  map (\(Capture s bf) -> CaptureEnv (StepEnv s b) bf) $ figCaptures fc
  where (FigureCtx _ b) = fc

evalCapture :: Board b => CaptureEnv b -> [FigureCtx b]
evalCapture (CaptureEnv se cf) =
  maybe [] (landFigCtx . StepEnv s) (setFigure b (pos cf) Nothing)
  where (StepEnv s b) = se

-- Capture tree
childCaptures :: Board b => CaptureEnv b -> [CaptureEnv b]
childCaptures =
  concat . map stepCaptures . evalCapture

expandCT :: Board b => CaptureEnv b -> (CaptureEnv b, [CaptureEnv b])
expandCT ce = (ce, childCaptures ce)

captureTree :: Board b => FigureCtx b -> CaptureTree b
captureTree se = CaptureTree se $ unfoldForest expandCT $ stepCaptures se
