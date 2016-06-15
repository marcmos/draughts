-- Steps (non-capture moves)
module Step where

import Data.Foldable (toList)
import Data.Maybe (mapMaybe)

import Board
import BoardChunk
import Neighborhood
import Figure

-- -----------------------------------------------------------------------------
-- Figure ranges
figureRange :: Board t => t BoardField -> FigureType -> Int
figureRange _ Pawn = 2
figureRange board King = size board

-- -----------------------------------------------------------------------------
-- Stepping

data Step = Step BoardFigure [BoardPosition] deriving Show
data FigureCtx t = FigureCtx BoardFigure (t BoardField)
data StepEnv t = StepEnv Step (t BoardField)
-- FIXME: common for Step and Capture, should be in separate file;
-- not in Move, because of circular dependency
data Path t = MoveStep BoardPosition BoardPosition (t BoardField) |
              MoveCapture BoardPosition [BoardPosition] BoardPosition
              (t BoardField)

instance Show (FigureCtx t) where
  show (FigureCtx bf _) = "FigureCtx (" ++ show bf ++ " *board*)"
instance Show (StepEnv t) where
  show (StepEnv s _) = "StepEnv (" ++ show s ++ ") *board*"
instance Show (Path t) where
  show (MoveStep p1 p2 _) = "MoveStep (" ++ show p1 ++ ") (" ++ show p2 ++ ") *board*"
  show _ = undefined

-- -----------------------------------------------------------------------------
stepMoves :: Board b => b BoardField -> BoardFigure -> [BoardField] ->
             [BoardField]
stepMoves b bf fields =
  maybe fields (filterBeforeChunk b bf fields)
  (closestFigure b bf fields)

forwardNeighborhood :: Neighborhood [n] -> Neighborhood [n]
forwardNeighborhood neigh =
  Neighborhood {origin = origin neigh,
                forward = forward neigh,
                backward = Diagonals [] []}

-- Removes illegal moves for non-capturing pawns: backward moves and steps
-- with distance larger than one.
stepFigCase :: Board b => b BoardField -> FigureType ->
               Neighborhood [BoardField] -> Neighborhood [BoardField]
stepFigCase b Pawn n =
  forwardNeighborhood (fmap (\bf -> constrainMaxDist b (origin n) bf 2) n)
stepFigCase _ King n = n

step :: Board b => b BoardField -> Neighborhood [BoardField] -> Maybe Step
step b n =
  let ft = (figureType . origin) n
      stepNbrhd = stepFigCase b ft $ stepMoves b (origin n) <$>
        orientNeighborhood n
      catSteps = (concat . toList) stepNbrhd
  in if null catSteps
     then Nothing
     else Just $ Step (origin n) (map pos catSteps)

landFigCtx :: Board b => StepEnv b -> [FigureCtx b]
landFigCtx (StepEnv (Step (BoardFigure srcPos f) landPosList) b) =
  mapMaybe buildCtx landPosList
  where buildCtx dstPos = do
          newBoard <- moveFigure b srcPos dstPos
          return $ FigureCtx (BoardFigure dstPos f) newBoard

-- Step path generation

-- FIXME: handle king transformation
steps :: Board b => FigureCtx b -> [Path b]
steps (FigureCtx bf b) =
  maybe [] (map buildPath) landingCtxs
  where (BoardFigure _ (Figure _ ft)) = bf
        nbr = neighborhood b bf $ figureRange b ft
        stepEnv = (flip StepEnv b) <$> step b nbr
        landingCtxs = landFigCtx <$> stepEnv
        buildPath (FigureCtx landBF landB) = MoveStep (pos bf) (pos landBF) landB
