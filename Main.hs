{-# LANGUAGE FlexibleContexts #-}
module Main where

import Board
import Figure
import GameTree

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

startTurn :: b BoardField -> Turn b
startTurn = \x -> Turn White x Nothing

showTurn :: Board b => Turn b -> IO ()
showTurn (Turn c b path) = do
  putStr . showBoard $ b
  putStrLn $ maybe "*unknown move* (initial board?)" show path
  putStrLn $ show c ++ " moves"

play :: Board b => Turn b -> IO (Turn b)
play t = do
  showTurn t
  _ <- getChar
  nt <- pure $ evalTurn 5 t
  play nt

main :: IO ()
main = do
  _ <- play $ startTurn exampleBoard
  return ()
