module GameTree where

import Data.Tree
import Data.List

import Board
import Move
import Figure
import Step

data Turn b = Turn FigureColor (b BoardField) (Maybe (Path b))
data VTurn b = VTurn Int (Turn b) deriving Show

instance Show (Turn b) where
  show (Turn fc _ path) = "Turn (" ++ show fc ++ ") *board* (" ++ show path ++
    ")"

evalTurn :: Board b =>  Int -> Turn b -> Turn b
evalTurn d t = (\(Turn c b p) -> Turn c (transformKings b) p) newTurn
  where (Node (VTurn _ newTurn) _) = evaluate d t

turns :: Board b => Turn b -> (Turn b, [Turn b])
turns t =
  (t, (\(path, b) -> Turn (oppositeColor c) b (Just path)) <$> childBoards)
  where
    (Turn c b _) = t
    startFields = filterColor c . filterOccupied $ b
    pathList = map (moves b) startFields
    childBoards = map (evalMove b) $ concat pathList

evalMove :: Board b => b BoardField -> Path b -> (Path b, b BoardField)
evalMove _ (MoveStep sp lp lb) = ((MoveStep sp lp lb), lb)
evalMove _ (MoveCapture sp ps lp lb) = ((MoveCapture sp ps lp lb), lb)

-- -----------------------------------------------------------------------------
-- Tree actions
type GTree b = Tree (Turn b)
type VTree b = Tree (VTurn b)

-- Generic
prune :: Int -> Tree a -> Tree a
prune 0 (Node l _) = Node l []
prune d (Node l sf) = Node l $ prune (d - 1) <$> sf

-- Game-tree related
sortV :: Forest (VTurn b) -> Forest (VTurn b)
sortV = (sortOn $ (\(VTurn v _) -> v) . rootLabel)

minV :: Forest (VTurn b) -> VTree b
minV = head . sortV

maxV :: Forest (VTurn b) -> (VTree b)
maxV = last . sortV

minV' :: VTree b -> VTree b
minV' (Node l []) = Node l []
minV' (Node _ sf) = minV sf

maxV' :: VTree b -> VTree b
maxV' (Node l []) = Node l []
maxV' (Node _ sf) = maxV $ sf

minleq :: Forest (VTurn b) -> VTree b -> Bool
minleq [] _ = False
minleq (n:ns) pot
  | num <= potV  = True
  | otherwise = minleq ns pot
  where (Node (VTurn num _) _) = n
        (Node (VTurn potV _) _) = pot

maxgeq :: Forest (VTurn b) -> (VTree b) -> Bool
maxgeq [] _ = False
maxgeq (n:ns) pot
  | num >= potV  = True
  | otherwise = maxgeq ns pot
  where (Node (VTurn num _) _) = n
        (Node (VTurn potV _) _) = pot

omitMin :: VTree b -> Forest (VTurn b) -> Forest (VTurn b)
omitMin _ [] = []
omitMin alpha (n:ns)
  | minleq (subForest n) alpha = omitMin alpha ns
  | otherwise                  = newAlpha : omitMin newAlpha ns
  where newAlpha = minV' n

omitMax :: VTree b -> Forest (VTurn b) -> Forest (VTurn b)
omitMax _ [] = []
omitMax beta (n:ns)
  | maxgeq (subForest n) beta = omitMax beta ns
  | otherwise                 = newBeta : omitMax newBeta ns
  where newBeta = maxV' n

mapmin :: Forest (VTurn b) -> Forest (VTurn b)
mapmin [] = []
mapmin (n:ns) =
   alpha : omitMin alpha ns
   where alpha = minV' n

mapmax :: Forest (VTurn b) -> Forest (VTurn b)
mapmax [] = []
mapmax (n:ns) =
  beta : omitMax beta ns
  where beta = maxV' n

maximise' :: VTree b -> VTree b
maximise' (Node vt []) = Node vt []
maximise' (Node (VTurn _ t) f) = Node (VTurn mv t) newForest
  where newForest = minimise' <$> f
        (Node (VTurn mv _) _) = maxV $ mapmin newForest

minimise' :: VTree b -> VTree b
minimise' (Node vt []) = Node vt []
minimise' (Node (VTurn _ t) f) = Node (VTurn mv t) newForest
  where newForest = maximise' <$> f
        (Node (VTurn mv _) _) = minV $ mapmax newForest

evaluate :: Board b => Int -> Turn b -> Tree (VTurn b)
evaluate d t = maxV' . maximise' . (fmap $ static c) . (prune d) .
  (unfoldTree turns) $ t
  where (Turn c _ _) = t

--------------------------------------------------------------------------------
-- Static evaluation
filterColor :: FigureColor -> [BoardFigure] -> [BoardFigure]
filterColor c bfl = filter (\x -> c == figureColor x) bfl

countColor :: Board a => a BoardField -> FigureColor -> Int
countColor b c =
  foldr (+) 0 $ figureWeight <$> (filterColor c . filterOccupied) b
  where figureWeight (BoardFigure _ (Figure _ King)) = 4
        figureWeight (BoardFigure _ (Figure _ Pawn)) = 1

static :: Board b => FigureColor -> Turn b -> VTurn b
static fc t =
  VTurn score t
  where (Turn _ b _) = t
        ourScore = countColor b fc
        opponentScore = countColor b (oppositeColor fc)
        score = ourScore - opponentScore
