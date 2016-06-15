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

--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
-- Capture tree
data CaptureTree b = CaptureTree (FigureCtx b) (Forest (FigureCtx b))
  deriving Show

childCtx :: Board b => FigureCtx b -> [FigureCtx b]
childCtx = concat . map evalCapture . stepCaptures

expandNode :: Board b => FigureCtx b -> (FigureCtx b, [FigureCtx b])
expandNode fc = (fc, childCtx fc)

captureTree :: Board b => FigureCtx b -> CaptureTree b
captureTree fc =
  CaptureTree fc $ unfoldForest expandNode $ childCtx fc

--------------------------------------------------------------------------------
-- Path build
leafPaths :: Board b => Tree (FigureCtx b) -> [[FigureCtx b]]
leafPaths tree =
  map reverse $ traverse [] tree
  where
    traverse path (Node fc []) = [fc:path]
    traverse path (Node fc xs) = concat $ map (traverse (fc:path)) xs

landing :: Board b => [FigureCtx b] ->
           ([BoardPosition], BoardPosition, b BoardField)
landing middles =
  (init middlesPosList, pos landBF, landB)
  where middlesPosList = (\(FigureCtx middle _) -> pos middle) <$> middles
        (FigureCtx landBF landB) = last middles

capturePaths :: Board b => FigureCtx b -> [Path b]
capturePaths fc =
  map (buildCapture . landing) . treeLandings . captureTree $ fc
  where (FigureCtx bf _) = fc
        buildCapture (middles, landPos, landBoard) = MoveCapture (pos bf)
          middles landPos landBoard
        treeLandings (CaptureTree _ f) = concat $ leafPaths <$> f
