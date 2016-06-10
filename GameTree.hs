module GameTree
where

import Data.Maybe
import Data.Tree
import Data.Foldable
import Data.List

import Board
import Field
import Move
import Figure

-- Move evaluation
-- TODO: handle Pawn -> King transformation
evalStep :: Board t => t BoardField -> Step -> [t BoardField]
evalStep b (Step bf fs) =
  mapMaybe ((moveFigure b (pos bf)) . pos) fs

evalCapture :: Board t => t BoardField -> Capture -> [t BoardField]
evalCapture b (Capture s cf) =
  maybe [] (\eb -> evalStep eb s) (setFigure b (pos cf) Nothing)

evalCaptures :: Board t => t BoardField -> [Capture] -> [t BoardField]
evalCaptures b cs =
  concat $ map (evalCapture b) cs

evalMove :: Board t => t BoardField -> Move -> [t BoardField]
evalMove b m =
  either (evalCaptures b) (evalStep b) m

-- -----------------------------------------------------------------------------
data Turn = Turn FigureColor [BoardField] deriving Show

evalTurn :: Int -> Turn -> Turn
evalTurn d t =
  let tree = (minmax tc (static tc)) . (prune d) . buildTree $ t
      (Turn tc _) = t
      extractS (Node (s, _) _) = s
      extractBoard (Node (_, b) _) = b
      extractC (Node _ c) = c
  in extractBoard . head . sortOn extractS . extractC $ tree

turns :: Turn -> (Turn, [Turn])
turns t =
  (t, Turn (oppositeColor c) <$> childBoards)
  where
    (Turn c b) = t
    startFields = filterColor c $ filterOccupied b
    moveList = catMaybes $ map (moves b) startFields
    childBoards = concat $ map (evalMove b) moveList

-- -----------------------------------------------------------------------------
-- Tree actions
type ValuedTurn = (Int, Turn)
type GTree = Tree Turn
type MMTree = Tree ValuedTurn

-- Generic
prune :: Int -> Tree a -> Tree a
prune 0 (Node l _) = Node l []
prune d (Node l sf) = Node l $ prune (d - 1) <$> sf

-- Game-tree related
buildTree :: Turn -> GTree
buildTree = unfoldTree turns

minmax :: FigureColor -> ([BoardField] -> Int) -> GTree -> MMTree
minmax _ s (Node (Turn c b) []) = Node (s b, (Turn c b)) []
minmax oc s (Node (Turn c b) f) =
  let translatedChildren = minmax oc s <$> f
      sval = (if oc /= c then maximum else minimum) svals
      svals = map (\(Node (x, _) _) -> x) translatedChildren
  in (Node (sval, (Turn c b)) translatedChildren)

--------------------------------------------------------------------------------
-- Static evaluation
filterColor :: FigureColor -> [BoardFigure] -> [BoardFigure]
filterColor c bfl = filter (\x -> c == figureColor x) bfl

countColor :: Board a => a BoardField -> FigureColor -> Int
countColor b c =
  length $ filter ((c ==) . figureColor) (filterOccupied b)

static :: Board a => FigureColor -> a BoardField -> Int
static oc b =
  (countColor b oc) - (countColor b (oppositeColor oc))
