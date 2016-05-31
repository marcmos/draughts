module Figure where

data FigureColor = Black | White deriving (Eq, Show)
data FigureType = Pawn | King deriving (Eq, Show)
data Figure = Figure FigureColor FigureType deriving (Eq, Show)

showFigure :: Figure -> Char
showFigure (Figure Black Pawn) = '\x26c2'
showFigure (Figure White Pawn) = '\x26c0'
showFigure (Figure Black King) = '\x26c3'
showFigure (Figure White King) = '\x26c1'

readFigure :: Char -> Figure
readFigure 'b' = Figure Black Pawn
readFigure 'w' = Figure White Pawn
readFigure 'B' = Figure Black King
readFigure 'W' = Figure White King
readFigure _ = error "Invalid figure"

isAlly :: Figure -> Figure -> Bool
isAlly (Figure color _) (Figure oppColor _) = color == oppColor

isEnemy :: Figure -> Figure -> Bool
isEnemy (Figure color _) (Figure oppColor _) = color /= oppColor

isPawn :: Figure -> Bool
isPawn (Figure _ Pawn) = True
isPawn _ = False
