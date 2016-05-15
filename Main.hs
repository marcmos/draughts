{-# LANGUAGE FlexibleContexts #-}
module Main where
import Board
import Field
import PrettyBoard

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
