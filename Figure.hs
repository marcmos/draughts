module Figure where

data Figure = Black | White | BlackKing | WhiteKing deriving (Eq, Show)

showFigure :: Figure -> Char
showFigure Black = 'b'
showFigure White = 'w'
showFigure BlackKing = 'B'
showFigure WhiteKing = 'W'

readFigure :: Char -> Figure
readFigure 'b' = Black
readFigure 'w' = White
readFigure 'B' = BlackKing
readFigure 'W' = WhiteKing
readFigure _ = error "Invalid figure"
