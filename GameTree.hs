module GameTree
where

import Data.Maybe
import Data.Tree
import Data.List

import Board
import Field
import Move
import Figure


-- -----------------------------------------------------------------------------
data Turn = Turn FigureColor [BoardField] deriving Show

evalTurn :: Int -> Turn -> Turn
evalTurn d t = b
  where (Node (VTurn _ b) _) = evaluate d t

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
type GTree = Tree Turn
data VTurn = VTurn Int Turn deriving Show
type VTree = Tree VTurn

-- Generic
prune :: Int -> Tree a -> Tree a
prune 0 (Node l _) = Node l []
prune d (Node l sf) = Node l $ prune (d - 1) <$> sf

-- Game-tree related
sortV :: Forest VTurn -> Forest VTurn
sortV = (sortOn $ (\(VTurn v _) -> v) . rootLabel)

minV :: Forest VTurn -> VTree
minV = head . sortV

maxV :: Forest VTurn -> VTree
maxV = last . sortV

minV' :: VTree -> VTree
minV' (Node l []) = Node l []
minV' (Node _ sf) = minV sf

maxV' :: VTree -> VTree
maxV' (Node l []) = Node l []
maxV' (Node _ sf) = maxV $ sf

minleq :: Forest VTurn -> VTree -> Bool
minleq [] _ = False
minleq (n:ns) pot
  | num <= potV  = True
  | otherwise = minleq ns pot
  where (Node (VTurn num _) _) = n
        (Node (VTurn potV _) _) = pot

maxgeq :: Forest VTurn -> VTree -> Bool
maxgeq [] _ = False
maxgeq (n:ns) pot
  | num >= potV  = True
  | otherwise = maxgeq ns pot
  where (Node (VTurn num _) _) = n
        (Node (VTurn potV _) _) = pot

omitMin :: VTree -> Forest VTurn -> Forest VTurn
omitMin _ [] = []
omitMin alpha (n:ns)
  | minleq (subForest n) alpha = omitMin alpha ns
  | otherwise                  = newAlpha : omitMin newAlpha ns
  where newAlpha = minV' n

omitMax :: VTree -> Forest VTurn -> Forest VTurn
omitMax _ [] = []
omitMax beta (n:ns)
  | maxgeq (subForest n) beta = omitMax beta ns
  | otherwise                 = newBeta : omitMax newBeta ns
  where newBeta = maxV' n

mapmin :: Forest VTurn -> Forest VTurn
mapmin [] = []
mapmin (n:ns) =
   alpha : omitMin alpha ns
   where alpha = minV' n

mapmax :: Forest VTurn -> Forest VTurn
mapmax [] = []
mapmax (n:ns) =
  beta : omitMax beta ns
  where beta = maxV' n

maximise' :: VTree -> VTree
maximise' (Node vt []) = Node vt []
maximise' (Node (VTurn _ t) f) = Node (VTurn mv t) newForest
  where newForest = minimise' <$> f
        (Node (VTurn mv _) _) = maxV $ mapmin newForest

minimise' :: VTree -> VTree
minimise' (Node vt []) = Node vt []
minimise' (Node (VTurn _ t) f) = Node (VTurn mv t) newForest
  where newForest = maximise' <$> f
        (Node (VTurn mv _) _) = minV $ mapmax newForest

evaluate :: Int -> Turn -> Tree VTurn
evaluate d t = maxV' . maximise' . (fmap $ static c) . (prune d) .
  (unfoldTree turns) $ t
  where (Turn c _) = t

--------------------------------------------------------------------------------
-- Static evaluation
filterColor :: FigureColor -> [BoardFigure] -> [BoardFigure]
filterColor c bfl = filter (\x -> c == figureColor x) bfl

countColor :: Board a => a BoardField -> FigureColor -> Int
countColor b c =
  foldr (+) 0 $ figureWeight <$> (filterColor c . filterOccupied) b
  where figureWeight (BoardFigure _ (Figure _ King)) = 4
        figureWeight (BoardFigure _ (Figure _ Pawn)) = 1

static :: FigureColor -> Turn -> VTurn
static fc t =
  VTurn score t
  where (Turn _ b) = t
        ourScore = countColor b fc
        opponentScore = countColor b (oppositeColor fc)
        score = ourScore - opponentScore
