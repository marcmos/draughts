module Move where

import Data.Maybe

import Board
import Field
import Figure

data Diagonals t = Diagonals {
  left :: t,
  right :: t
  } deriving Show

instance Functor Diagonals where
  fmap f diag =
    Diagonals {left = f (left diag),
               right = f (right diag)}

data Neighborhood t = Neighborhood {
  origin :: BoardFigure,
  forward :: Diagonals t,
  backward :: Diagonals t
  } deriving Show

instance Functor Neighborhood where
  fmap f n =
    Neighborhood {origin = origin n,
                  forward = fmap f $ forward n,
                  backward = fmap f $ backward n}

figureRange :: Board t => t -> FigureType -> Int
figureRange _ Pawn = 2
figureRange board King = size board

diagonalPositions :: BoardPosition -> Int -> (BoardRow -> BoardRow -> BoardRow)
                  -> (BoardColumn -> BoardColumn -> BoardColumn) ->
                     [BoardPosition]
diagonalPositions (a, b) radius rowOp colOp =
  [(rowOp a x, colOp b x) | x <- [1 .. radius]]

forwardPositions :: Int -> BoardPosition -> Diagonals [BoardPosition]
forwardPositions radius (a, b) =
  let diagMovesForward = diagonalPositions (a, b) radius (-)
  in Diagonals {left = diagMovesForward (-),
                right = diagMovesForward (+)}

backwardPositions :: Int -> BoardPosition -> Diagonals [BoardPosition]
backwardPositions radius (a, b) =
  let diagMovesBackward = diagonalPositions (a, b) radius (+)
  in Diagonals {left = diagMovesBackward (-),
                right = diagMovesBackward (+)}

neighborhood :: Board t => t -> BoardPosition -> Int ->
                Maybe (Neighborhood [BoardField])
neighborhood board pos radius =
  let toFields = fmap $ getFields board
  in do
    originFigure <- getFigure board pos
    return $ Neighborhood {origin = originFigure,
                           forward = toFields (forwardPositions radius pos),
                           backward = toFields (backwardPositions radius pos)}

orientNeighborhood :: Neighborhood a -> Neighborhood a
orientNeighborhood (Neighborhood (BoardFigure p (Figure Black fg)) fw bw) =
  Neighborhood (BoardFigure p (Figure Black fg)) bw fw
orientNeighborhood n = n

closestPosDist :: BoardPosition -> [BoardPosition] -> Maybe Int
closestPosDist orig positions =
  let distances = mapMaybe (getDistance orig) positions
  in if null distances then Nothing else Just (minimum distances)

constrainDist :: BoardChunk c => (Maybe Int -> Maybe Int -> Bool) ->
                 BoardPosition -> [c] -> Int -> [c]
constrainDist f refPos posList dist =
  filter (\chunk -> (getDistance refPos (pos chunk)) `f` Just dist) posList

constrainMinDist :: BoardChunk c => BoardPosition -> [c] -> Int -> [c]
constrainMinDist = constrainDist (>)

constrainMaxDist :: BoardChunk c => BoardPosition -> [c] -> Int -> [c]
constrainMaxDist = constrainDist (<)

closestChunk :: BoardChunk c => BoardPosition -> [c] -> Maybe (c, Int)
closestChunk orig fields =
  let positions = map pos fields
  in do
    dist <- closestPosDist orig positions
    return $ (head $ constrainMaxDist orig fields (dist + 1), dist)

closestFigure :: BoardPosition -> [BoardField] -> Maybe (BoardFigure, Int)
closestFigure p fields =
  closestChunk p (filterOccupied fields)

captureMoves :: BoardFigure -> [BoardField] -> [BoardField]
captureMoves (BoardFigure origPos origFig) fields =
  let
    res = do
      (BoardFigure clPos clFig, dist) <- closestFigure origPos fields
      gotEnemy <- if isEnemy origFig clFig then pure True else Nothing
      distantFields <- pure $ constrainMinDist origPos fields dist
      (_, nextDist) <- closestFigure clPos distantFields
      finalFields <- pure $ constrainMaxDist clPos distantFields nextDist
      return $ if gotEnemy then Just finalFields else Nothing
  in maybe [] fromJust res

nonCaptureMoves :: BoardFigure -> [BoardField] -> [BoardField]
nonCaptureMoves (BoardFigure origPos _) fields =
  let closest = closestFigure origPos fields
  in maybe fields (\(_, dist) -> constrainMaxDist origPos fields dist) closest

-- FIXME: lol, just implement Foldable instance
notCapturing :: Neighborhood [BoardField] -> Bool
notCapturing neigh =
  foldr (&&) True [null $ (left . forward) neigh,
                   null $ (right . forward) neigh,
                   null $ (left . backward) neigh,
                   null $ (right . backward) neigh]

forwardNeighborhood :: Neighborhood [n] -> Neighborhood [n]
forwardNeighborhood neigh =
  Neighborhood {origin = origin neigh,
                forward = forward neigh,
                backward = Diagonals [] []}

-- Removes illegal moves for non-capturing pawns: backward moves and steps
-- with distance larger than one.
nonCapFigCase :: FigureType -> Neighborhood [BoardField] ->
                 Neighborhood [BoardField]
nonCapFigCase Pawn n =
  let p = (pos . origin) n
  in forwardNeighborhood (fmap (\z -> constrainMaxDist p z 2) n)
nonCapFigCase King n = n

getMoves :: Board t => t -> BoardPosition -> Maybe (Neighborhood [BoardField])
getMoves board orig =
  do
    bf <- getFigure board orig
    (Figure _ ft) <- pure $ figure bf
    range <- pure $ figureRange board ft
    neighbors <- orientNeighborhood <$> neighborhood board orig range
    captures <- pure $ captureMoves bf <$> neighbors
    nonCaptures <- pure $ nonCaptureMoves bf <$> neighbors
    allowedNonCaptures <- pure $ nonCapFigCase ft nonCaptures
    return $ if notCapturing captures then allowedNonCaptures else captures
