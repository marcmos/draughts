module GameTree
where

import Data.Maybe
import Data.Tree
import Data.Foldable

import Board
import Field
import Move

-- TODO: handle Pawn -> King transformation
evalStep :: Board t => t -> Step -> [t]
evalStep b (Step bf fs) =
  mapMaybe ((moveFigure b (pos bf)) . pos) fs

evalCapture :: Board t => t -> Capture -> [t]
evalCapture b (Capture s cf) =
  maybe [] (\eb -> evalStep eb s) (setFigure b (pos cf) Nothing)

evalCaptures :: Board t => t -> [Capture] -> [t]
evalCaptures b cs =
  concat $ map (evalCapture b) cs

evalMove :: Board t => t -> Move -> [t]
evalMove b m =
  either (evalCaptures b) (evalStep b) m
