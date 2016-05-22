module Figure where

data FigureColor = Black | White deriving (Eq, Show)
data FigureType = Pawn | King deriving (Eq, Show)
type Figure = (FigureColor, FigureType)

showFigure :: Figure -> Char
showFigure (Black, Pawn) = '\x26c2'
showFigure (White, Pawn) = '\x26c0'
showFigure (Black, King) = '\x26c1'
showFigure (White, King) = '\x26c3'

readFigure :: Char -> Figure
readFigure 'b' = (Black, Pawn)
readFigure 'w' = (White, Pawn)
readFigure 'B' = (Black, King)
readFigure 'W' = (White, King)
readFigure _ = error "Invalid figure"

isAllied :: FigureColor -> FigureColor -> Bool
isAllied color oppColor = color == oppColor

isAlliedFigure :: Figure -> Figure -> Bool
isAlliedFigure (color, _) (oppColor, _) = isAllied color oppColor
