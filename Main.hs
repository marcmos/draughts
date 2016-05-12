module Main where
import Board

-- TODO:
-- implement board using hash table
-- use DataTree in min-max algorithm

exampleBoardStr =
  ".b.b.b.b\n" ++
  "b.b.b.b.\n" ++
  ".b.b.b.b\n" ++
  "........\n" ++
  "........\n" ++
  "w.w.w.w.\n" ++
  ".w.w.w.w\n" ++
  "w.w.w.w."

exampleBoard = readBoard exampleBoardStr

main =
  return ()
