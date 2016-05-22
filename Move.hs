module Move where
import Data.Maybe
import Board
import Figure
import Field

-- -----------------------------------------------------------------------------
-- Move generation

-- generates only positions, without knowledge of board size
-- ,,forward'' and ,,backward'' for these functions is meant in white's POV

-- range constraint in single step
getMoveRange :: Board t => t -> FigureType -> Int
getMoveRange _ Pawn = 2
getMoveRange board King = size board

-- FIXME: write type
-- genSpectralMove (a, b) radius rowOp colOp =
--   [(rowOp a x, colOp b x) | x <- [1 .. radius]]

isSpectralMove :: BoardPosition -> BoardPosition -> Bool
isSpectralMove (origRow, origCol) (row, col) =
  abs (origRow - row) == abs (origCol - col)

-- genForwardMoves :: Int -> BoardPosition -> [[BoardPosition]]
-- genForwardMoves radius (a, b) =
--   let forwardRowMove = genSpectralMove (a, b) radius (-)
--   in [forwardRowMove (-), forwardRowMove (+)]

-- genBackwardMoves :: Int -> BoardPosition -> [[BoardPosition]]
-- genBackwardMoves radius (a, b) =
--   let backwardRowMove = genSpectralMove (a, b) radius (+)
--   in [backwardRowMove (-), backwardRowMove (+)]

-- returned directions are ,,color directed''; first list is move ,,forward''
-- in means for given color, second is ,,backward''
-- genSpectralMoves :: Int -> FigureColor -> BoardPosition -> [[[BoardPosition]]]
-- genSpectralMoves radius color (a, b) =
--   let whiteForwardMoves = genForwardMoves radius (a, b)
--       whiteBackwardMoves = genBackwardMoves radius (a, b)
--   in if color == White
--      then [whiteForwardMoves, whiteBackwardMoves]
--      else [whiteBackwardMoves, whiteForwardMoves]

-- getLongestMoves :: Board t => t -> BoardPosition -> Figure ->
--                      [[[BoardPosition]]]
-- getLongestMoves board pos (color, figure) =
--   let radius = if figure == Pawn then 2 else (size board)
--       moves = genSpectralMoves radius color pos
--   in mapDirectedPos moves map (\col -> filter (isPosInBoard board) col)

-- -----------------------------------------------------------------------------
-- Functions applying to ,,directed positions''
-- first dimension: vertical direction
-- second dimension: horizontal direction
-- third dimension: moves

type BoardFigure = (BoardPosition, Figure)

-- FIXME: write type
mapDirectedPos pos rowFunc f =
  rowFunc (\row -> map f row) pos

-- returns distance to closest position
-- FIXME: should check, if position is valid (spectral)
closestPosDist :: BoardPosition -> [BoardPosition] -> Maybe Int
closestPosDist orig positions = do
  dist <- mapM (getDistance orig) positions
  return (minimum dist)

-- filters out fields more distant than specified
constrainDist :: BoardPosition -> [BoardField] -> Int -> [BoardField]
constrainDist refPos posList dist =
  filter (\(pos, _) -> (getDistance refPos pos) < Just dist) posList

-- filters out fields blocked by out allies, as we can't jump over them
allyBlockedMoves :: BoardFigure -> [BoardField] -> [BoardField]
allyBlockedMoves (origPos, origFig) fields =
  let fieldAllied (_, Field fig) = maybe False (isAlliedFigure origFig) fig
      alliedFields = filter fieldAllied fields
      alliedPos = fst $ unzip alliedFields
      closestAllyDist = closestPosDist origPos alliedPos
      validMoves = filter (\(pos, _) -> isSpectralMove origPos pos) fields
  in maybe validMoves (constrainDist origPos validMoves) closestAllyDist

--

-- closestFigureDist :: BoardPosition -> [BoardFigure] -> Maybe Int
-- closestFigureDist pos neighbors =
--   let dist = map (\(neighPos, _) -> getDistance pos neighPos) neighbors
--   in if null dist then Nothing else minimum dist
