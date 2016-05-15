module PrettyBoard (prettyShowBoard) where
import Board

genColCaption :: String
genColCaption = "  12345678"

genRowCaptions :: [String]
genRowCaptions =
  [show num | num <- [(1 :: Int) ..]]

addRowCaptions :: [String] -> [String]
addRowCaptions strList =
  let captionedRows = zip genRowCaptions strList
  in map (\(caption, line) -> caption ++ " " ++ line) captionedRows

addColCaptions :: [String] -> [String]
addColCaptions strList =
  let colCaption = genColCaption
  in [colCaption] ++ strList ++ [colCaption]

addCaptions :: [String] -> [String]
addCaptions strList =
  (addColCaptions . addRowCaptions) strList

prettyShowBoard :: Board b => b -> String
prettyShowBoard board =
  unlines ((addCaptions . showLines) board)
