{-# LANGUAGE FlexibleContexts #-}
module Main where
import Board
import Field

-- for interactive use
import Figure
--import PrettyBoard
import Move
import GameTree
import Data.Maybe
import Control.Monad
import Data.Tree

exampleBoardStr :: String
exampleBoardStr = unlines
  [".b.b.b.b",
   "b.b.b.b.",
   ".b.b.b.b",
   "........",
   "........",
   "w.w.w.w.",
   ".w.w.w.w",
   "w.w.w.w."]

readListBoard :: Board [] => String -> [BoardField]
readListBoard = readBoard

exampleBoard :: [BoardField]
exampleBoard = readListBoard exampleBoardStr

startTurn :: [BoardField] -> Turn
startTurn = Turn White

showTurn :: Turn -> IO ()
showTurn (Turn c b) = do
  putStr . showBoard $ b
  putStrLn $ show c ++ " moves"

play :: Turn -> IO Turn
play t = do
  showTurn t
  _ <- getChar
  nt <- pure $ evalTurn 4 t
  play nt

main :: IO ()
main = do
  _ <- play $ startTurn exampleBoard
  return ()
