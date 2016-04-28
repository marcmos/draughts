import Data.List

data Figure = Black | White | BlackKing | WhiteKing
data Field = Field (Maybe Figure)
type BoardLine = [Field]
type Board = [BoardLine]

prepend c x = [c, x]
append c x = [x, c]

indiceList l =
  zip [1 ..] l

showFigure Black = 'b'
showFigure White = 'w'
showFigure BlackKing = 'B'
showFigure WhiteKing = 'W'

showField (Field Nothing) = '.'
showField (Field (Just figure)) = showFigure figure

showBoardLine pad line =
  concat (map (pad . showField) line)

mapOddEven evenf oddf l =
  map (\x -> let num = fst x
                 elem = snd x
             in if even num
                then evenf elem
                else oddf elem) (indiceList l)

showBoard pad board =
  unlines (mapOddEven (showBoardLine (append pad)) (showBoardLine (prepend pad)) board)
  
exampleBoard =
  [[Field (Just Black), Field Nothing, Field (Just WhiteKing), Field (Just BlackKing)],
   [Field Nothing, Field Nothing, Field (Just White), Field (Just Black)],
   [Field Nothing, Field Nothing, Field (Just White), Field (Just Black)]]
