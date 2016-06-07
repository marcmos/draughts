{-# LANGUAGE FlexibleContexts #-}
module Main where
import Board
import Field

-- for interactive use
import Figure
import PrettyBoard
import Move
--import GameTree
import Data.Maybe

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

readListBoard :: Board [[Field]] => String -> [[Field]]
readListBoard = readBoard

exampleBoard :: [[Field]]
exampleBoard = readListBoard exampleBoardStr

main :: IO ()
main =
  return ()
