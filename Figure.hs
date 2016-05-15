module Figure where

-- Split into Man and King classes
data Figure = Black | White | BlackKing | WhiteKing deriving (Eq, Show)

showFigure :: Figure -> Char
showFigure Black = '\x26c2'
showFigure White = '\x26c0'
showFigure BlackKing = '\x26c1'
showFigure WhiteKing = '\x26c3'

readFigure :: Char -> Figure
readFigure 'b' = Black
readFigure 'w' = White
readFigure 'B' = BlackKing
readFigure 'W' = WhiteKing
readFigure _ = error "Invalid figure"
