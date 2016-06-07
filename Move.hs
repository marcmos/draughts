module Move where

import Data.Maybe
import Data.List (sortOn, intersect)
import Data.Foldable (toList)

import Board
import Field ()
import Figure

data Diagonals t = Diagonals {
  left :: t,
  right :: t
  } deriving Show

instance Functor Diagonals where
  fmap f diag =
    Diagonals {left = f (left diag),
               right = f (right diag)}

instance Foldable Diagonals where
  foldr f z t = f (left t) (f (right t) z)

-- FIXME: this assumes, that origin is BoardFigure, but generally it is a Chunk
data Neighborhood t = Neighborhood {
  origin :: BoardFigure,
  forward :: Diagonals t,
  backward :: Diagonals t
  } deriving Show

instance Functor Neighborhood where
  fmap f n =
    Neighborhood {origin = origin n,
                  forward = fmap f $ forward n,
                  backward = fmap f $ backward n}

instance Foldable Neighborhood where
  foldr f z t =
    let lf = (left . forward) t
        rf = (right . forward) t
        lb = (left . backward) t
        rb = (right . backward) t
    in f lf (f rf (f lb (f rb z)))

-- -----------------------------------------------------------------------------
-- Figure ranges
figureRange :: Board t => t -> FigureType -> Int
figureRange _ Pawn = 2
figureRange board King = size board

-- -----------------------------------------------------------------------------
-- Neighborhood generation
diagonalPositions :: BoardPosition -> Int -> (BoardRow -> BoardRow -> BoardRow)
                  -> (BoardColumn -> BoardColumn -> BoardColumn) ->
                     [BoardPosition]
diagonalPositions (a, b) radius rowOp colOp =
  [(rowOp a x, colOp b x) | x <- [1 .. radius]]

forwardPositions :: Int -> BoardPosition -> Diagonals [BoardPosition]
forwardPositions radius (a, b) =
  let diagMovesForward = diagonalPositions (a, b) radius (-)
  in Diagonals {left = diagMovesForward (-),
                right = diagMovesForward (+)}

backwardPositions :: Int -> BoardPosition -> Diagonals [BoardPosition]
backwardPositions radius (a, b) =
  let diagMovesBackward = diagonalPositions (a, b) radius (+)
  in Diagonals {left = diagMovesBackward (-),
                right = diagMovesBackward (+)}

neighborhood :: Board t => t -> BoardFigure -> Int -> Neighborhood [BoardField]
neighborhood b bf radius =
  let p = pos bf
      toFields = fmap $ getFields b
  in Neighborhood {origin = bf,
                   forward = toFields $ forwardPositions radius p,
                   backward = toFields $ backwardPositions radius p}

orientNeighborhood :: Neighborhood a -> Neighborhood a
orientNeighborhood (Neighborhood (BoardFigure p (Figure Black fg)) fw bw) =
  Neighborhood (BoardFigure p (Figure Black fg)) bw fw
orientNeighborhood n = n

flattenNbrhd :: Neighborhood [a] -> [a]
flattenNbrhd = (concat . toList)

--------------------------------------------------------------------------------
-- Chunk distance calculation
chunkDistance :: (BoardChunk c1, BoardChunk c2) => c1 -> c2 -> Int
chunkDistance c1 c2 =
  let (fstRow, fstCol) = pos c1
      (sndRow, sndCol) = pos c2
      rowDist = abs $ fstRow - sndRow
      colDist = abs $ fstCol - sndCol
  in if rowDist == colDist
     then rowDist
     else error "Move.chunkDistance: column and row distance are different"

closestChunks :: (BoardChunk c1, BoardChunk c2) => c1 -> [c2] -> [(c2, Int)]
closestChunks c l =
  sortOn snd (zip l (map (chunkDistance c) l))

-- FIXME: rewrite it
constrainDist :: (BoardChunk c1, BoardChunk c2) => (Int -> Int -> Bool) ->
                 c1 -> [c2] -> Int -> [c2]
constrainDist f refPos posList dist =
  filter (\chunk -> (chunkDistance refPos chunk) `f` dist) posList

constrainMinDist :: (BoardChunk c1, BoardChunk c2) => c1 -> [c2] -> Int -> [c2]
constrainMinDist = constrainDist (>)

constrainMaxDist :: (BoardChunk c1, BoardChunk c2) => c1 -> [c2] -> Int -> [c2]
constrainMaxDist = constrainDist (<)

filterBeforeChunk :: (BoardChunk c1, BoardChunk c2, BoardChunk c3) =>
                     c1 -> [c2] -> c3 -> [c2]
filterBeforeChunk orig chunks dest =
  constrainMaxDist orig chunks (chunkDistance orig dest)

filterAfterChunk :: (BoardChunk c1, BoardChunk c2, BoardChunk c3) =>
                    c1 -> [c2] -> c3 -> [c2]
filterAfterChunk orig chunks dest =
  constrainMinDist orig chunks (chunkDistance orig dest)

closestFigures :: BoardChunk c => c -> [BoardField] -> [BoardFigure]
closestFigures chunk =
  fst . unzip . (closestChunks chunk) . (mapMaybe fromBoardField)

closestFigure :: BoardChunk c => c -> [BoardField] -> Maybe BoardFigure
closestFigure c bf =
  let figures = closestFigures c bf
  in if null figures then Nothing else Just $ head figures

-- -----------------------------------------------------------------------------
data Step = Step BoardFigure [BoardField] deriving Show
data Capture = Capture Step BoardFigure deriving Show
type Move = Either [Capture] Step
-- type Turn = Either [Step] [Capture]

-- -----------------------------------------------------------------------------
-- Captures

-- From our POV gives valid landing fields
jumpFields :: BoardFigure -> [BoardField] -> Maybe (BoardFigure, [BoardField])
jumpFields bf f =
  if null figures || null landings then Nothing else Just (closest, landings)
  where figures = closestFigures bf f
        closest = head figures
        after = filterAfterChunk bf f closest
        beforeNext = filterBeforeChunk bf f (figures !! 1)
        landings = if length figures < 2 || null beforeNext
                   then after else intersect after beforeNext

capture :: BoardFigure -> [BoardField] -> Maybe Capture
capture bf fs = do
  (jf, ls) <- jumpFields bf fs
  _ <- if isAlly (figure bf) (figure jf) then Nothing else pure True
  return $ Capture (Step bf ls) jf

-- -----------------------------------------------------------------------------
-- Steps (non-capture moves)
forwardNeighborhood :: Neighborhood [n] -> Neighborhood [n]
forwardNeighborhood neigh =
  Neighborhood {origin = origin neigh,
                forward = forward neigh,
                backward = Diagonals [] []}

stepMoves :: BoardFigure -> [BoardField] -> [BoardField]
stepMoves bf fields =
  maybe fields (filterBeforeChunk bf fields) (closestFigure bf fields)

-- Removes illegal moves for non-capturing pawns: backward moves and steps
-- with distance larger than one.
stepFigCase :: FigureType -> Neighborhood [BoardField] ->
                 Neighborhood [BoardField]
-- FIXME: refactor removing unnecessary lambda
stepFigCase Pawn n =
  forwardNeighborhood (fmap (\z -> constrainMaxDist (origin n) z 2) n)
stepFigCase King n = n

step :: Neighborhood [BoardField] -> Maybe Step
step n =
  let ft = (figureType . origin) n
      stepNbrhd = stepFigCase ft (stepMoves (origin n) <$> n)
      catSteps = (concat . toList) stepNbrhd
  in if null catSteps then Nothing else pure $ Step (origin n) catSteps

moves :: Board t => t -> BoardFigure -> Maybe Move
moves b bf
  | null capt == False = pure $ Left $ capt
  | isJust steps       = pure $ Right $ fromJust steps
  | otherwise          = Nothing
  where nbrs = neighborhood b bf (figureRange b (figureType bf))
        capt = (catMaybes . toList) $ capture bf <$> nbrs
        steps = step nbrs
