data Figure = Black | White | BlackKing | WhiteKing deriving Show
data Field = Field (Maybe Figure) deriving Show
data Victory = WhiteVictory | BlackVictory
type BoardLine = [Field]
type Board = [BoardLine]

-- politicalCorrectness :: Victory -> Victory
-- politicalCorrectness v =
--   if v == WhiteVictory then
--     debug "THAT'S RACIST"
--     BlackVictory
--   else
--     BlackVictory

prepend c x = [c, x]
append c x = [x, c]

indiceList l =
  zip [1 ..] l

every p l =
  map snd (filter (\x -> (p . fst) x) (indiceList l))

mapOddEven evenf oddf l =
  map (\x -> let num = fst x
                 elem = snd x
             in if even num
                then evenf elem
                else oddf elem) (indiceList l)

showFigure Black = 'b'
showFigure White = 'w'
showFigure BlackKing = 'B'
showFigure WhiteKing = 'W'

readFigure 'b' = Black
readFigure 'w' = White
readFigure 'B' = BlackKing
readFigure 'W' = WhiteKing

showField (Field Nothing) = '.'
showField (Field (Just figure)) = showFigure figure

readField '.' = Field Nothing
readField c = Field (Just (readFigure c))

showBoardLine pad line =
  concat (map (pad . showField) line)

showBoard pad board =
  unlines (mapOddEven (showBoardLine (append pad)) (showBoardLine (prepend pad)) board)

readBoardLine parity str =
  map readField (every parity str)

readBoard str =
  mapOddEven (readBoardLine odd) (readBoardLine even) (lines str)

exampleBoardStr =
  ".b.b.b.b\n" ++
  "b.b.b.b.\n" ++
  ".b.b.b.b\n" ++
  "........\n" ++
  "........\n" ++
  "w.w.w.w.\n" ++
  ".w.w.w.w\n" ++
  "w.w.w.w."

exampleBoardLineStr = head (lines exampleBoardStr)

exampleBoard =
  [[Field (Just Black), Field Nothing, Field (Just WhiteKing), Field (Just BlackKing)],
   [Field Nothing, Field Nothing, Field (Just White), Field (Just Black)],
   [Field Nothing, Field Nothing, Field (Just White), Field (Just Black)]]
