module Move where
import Board

-- data Move = ForwardLeft | BackwardLeft | ForwardRight | BackwardRight

-- move generation
genSpectralMoves :: Int -> BoardPosition -> [BoardPosition]
genSpectralMoves radius (a, b) =
  concat [[(a + x, b + x),
           (a - x, b - x),
           (a + x, b - x),
           (a - x, b + x)] | x <- [1 .. radius]]

genMoves :: Board t => t -> BoardPosition -> [BoardPosition]
genMoves board (a, b) =
  filter (isPosInBoard board) (genSpectralMoves (size board) (a, b))
