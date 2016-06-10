module Move where

import Data.Maybe
import Data.List (sortOn, intersect, unfoldr)
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
figureRange :: Board t => t BoardField -> FigureType -> Int
figureRange _ Pawn = 2
figureRange board King = size board

-- -----------------------------------------------------------------------------
-- Neighborhood generation
validate2D :: Int -> (Int, Int) -> Maybe (Int, Int)
validate2D base (col, row) =
  if col >= 1 && col <= base &&
     row >= 1 && row <= base &&
     odd col /= odd row
  then Just (col, row)
  else Nothing

to2D :: Int -> Int -> Maybe (Int, Int)
to2D base p
  | odd row   = validate2D base (row + 1, col * 2 + 1)
  | otherwise = validate2D base (row + 1, col * 2 + 2)
  where (row, col) = divMod (p - 1) (base `quot` 2)

toPos :: Int -> (Int, Int) -> Maybe Int
toPos base (row, col)
  | row < 1 || row > base   = Nothing
  | col < 1 || col > base   = Nothing
  | odd row == odd col      = Nothing
  | otherwise               = Just $ p
  where normCol = if odd row then col - 1 else col
        p = (row - 1) * (base `quot` 2) + normCol `quot` 2 + 1

neighborPos :: Int -> Int -> (Int -> Int -> Int) -> (Int -> Int -> Int) ->
               Maybe Int
neighborPos base p rowf colf = do
  (row, col) <- to2D base p
  toPos base ((row `rowf` 1), (col `colf` 1))

forwardLeft :: Int -> Int -> Maybe Int
forwardLeft base p = neighborPos base p (-) (-)

forwardRight :: Int -> Int -> Maybe Int
forwardRight base p = neighborPos base p (-) (+)

backwardLeft :: Int -> Int -> Maybe Int
backwardLeft base p = neighborPos base p (+) (-)

backwardRight :: Int -> Int -> Maybe Int
backwardRight base p = neighborPos base p (+) (+)

unfoldrDir :: Int -> Int -> (Int -> Int -> Maybe Int) -> [Int]
unfoldrDir base p posf =
  let nextSeed = fmap (\x -> (x, x))
  in unfoldr (nextSeed . (posf base)) p

forwardPositions :: Int -> Int -> Diagonals [Int]
forwardPositions base p =
  Diagonals (unfoldrDir base p forwardLeft) (unfoldrDir base p forwardRight)

backwardPositions :: Int -> Int -> Diagonals [Int]
backwardPositions base p =
  Diagonals (unfoldrDir base p backwardLeft) (unfoldrDir base p backwardRight)

neighborhood :: Board t => t BoardField -> BoardFigure -> Int ->
                Neighborhood [BoardField]
neighborhood b bf radius =
  let p = pos bf
      toFields = fmap $ getFields b
      limit = take radius
  in Neighborhood {origin = bf,
                   forward = toFields $ limit <$> forwardPositions (size b) p,
                   backward = toFields $ limit <$> backwardPositions (size b) p}

orientNeighborhood :: Neighborhood a -> Neighborhood a
orientNeighborhood (Neighborhood (BoardFigure p (Figure Black fg)) fw bw) =
  Neighborhood (BoardFigure p (Figure Black fg)) bw fw
orientNeighborhood n = n

flattenNbrhd :: Neighborhood [a] -> [a]
flattenNbrhd = (concat . toList)

--------------------------------------------------------------------------------
-- Chunk distance calculation

-- TODO: these calculations are parametrized by board size;
--       consider keeping the size of boards in BoardChunk type itself
chunkDistance :: (BoardChunk c1, BoardChunk c2) => Int -> c1 -> c2 -> Int
chunkDistance bsize c1 c2 =
  let (fstRow, fstCol) = fromJust $ to2D bsize $ pos c1
      (sndRow, sndCol) = fromJust $ to2D bsize $ pos c2
      rowDist = abs $ fstRow - sndRow
      colDist = abs $ fstCol - sndCol
  in if rowDist == colDist
     then rowDist
     else error "Move.chunkDistance: column and row distance are different"

closestChunks :: (BoardChunk c1, BoardChunk c2) => Int-> c1 -> [c2] ->
                 [(c2, Int)]
closestChunks bsize c l =
  sortOn snd (zip l (map (chunkDistance bsize c) l))

constrainDist :: (BoardChunk c1, BoardChunk c2) =>
                 (Int -> Int -> Bool) -> Int -> c1 -> [c2] -> Int -> [c2]
constrainDist f bsize refPos posList dist =
  filter (\chunk -> (chunkDistance bsize refPos chunk) `f` dist) posList

constrainMinDist :: (BoardChunk c1, BoardChunk c2) => Int -> c1 -> [c2] ->
                    Int -> [c2]
constrainMinDist = constrainDist (>)

constrainMaxDist :: (BoardChunk c1, BoardChunk c2) => Int -> c1 -> [c2] ->
                    Int -> [c2]
constrainMaxDist = constrainDist (<)

filterBeforeChunk :: (BoardChunk c1, BoardChunk c2, BoardChunk c3) =>
                     Int -> c1 -> [c2] -> c3 -> [c2]
filterBeforeChunk bsize orig chunks dest =
  constrainMaxDist bsize orig chunks (chunkDistance bsize orig dest)

filterAfterChunk :: (BoardChunk c1, BoardChunk c2, BoardChunk c3) =>
                    Int -> c1 -> [c2] -> c3 -> [c2]
filterAfterChunk bsize orig chunks dest =
  constrainMinDist bsize orig chunks (chunkDistance bsize orig dest)

closestFigures :: BoardChunk c => Int -> c -> [BoardField] -> [BoardFigure]
closestFigures bsize chunk =
  fst . unzip . (closestChunks bsize chunk) . (mapMaybe fromBoardField)

closestFigure :: BoardChunk c => Int -> c -> [BoardField] -> Maybe BoardFigure
closestFigure bsize c bf =
  let figures = closestFigures bsize c bf
  in if null figures then Nothing else Just $ head figures

-- -----------------------------------------------------------------------------
data Step = Step BoardFigure [BoardField] deriving Show
data Capture = Capture Step BoardFigure deriving Show
type Move = Either [Capture] Step
-- type Turn = Either [Step] [Capture]

-- -----------------------------------------------------------------------------
-- Captures

-- From our POV gives valid landing fields
jumpFields :: Int -> BoardFigure -> [BoardField] ->
              Maybe (BoardFigure, [BoardField])
jumpFields bsize bf f =
  if null figures || null landings then Nothing else Just (closest, landings)
  where figures = closestFigures bsize bf f
        closest = head figures
        after = filterAfterChunk bsize bf f closest
        beforeNext = filterBeforeChunk bsize bf f (figures !! 1)
        landings = if length figures < 2 || null beforeNext
                   then after else intersect after beforeNext

capture :: Int -> BoardFigure -> [BoardField] -> Maybe Capture
capture bsize bf fs = do
  (jf, ls) <- jumpFields bsize bf fs
  _ <- if isAlly (figure bf) (figure jf) then Nothing else pure True
  return $ Capture (Step bf ls) jf

-- -----------------------------------------------------------------------------
-- Steps (non-capture moves)
forwardNeighborhood :: Neighborhood [n] -> Neighborhood [n]
forwardNeighborhood neigh =
  Neighborhood {origin = origin neigh,
                forward = forward neigh,
                backward = Diagonals [] []}

stepMoves :: Int -> BoardFigure -> [BoardField] -> [BoardField]
stepMoves bsize bf fields =
  maybe fields (filterBeforeChunk bsize bf fields)
  (closestFigure bsize bf fields)

-- Removes illegal moves for non-capturing pawns: backward moves and steps
-- with distance larger than one.
stepFigCase :: Int -> FigureType -> Neighborhood [BoardField] ->
               Neighborhood [BoardField]
stepFigCase bsize Pawn n =
  forwardNeighborhood (fmap (\bf -> constrainMaxDist bsize (origin n) bf 2) n)
stepFigCase _ King n = n

step :: Int -> Neighborhood [BoardField] -> Maybe Step
step bsize n =
  let ft = (figureType . origin) n
      stepNbrhd = stepFigCase bsize ft (stepMoves bsize (origin n) <$>
                                        orientNeighborhood n)
      catSteps = (concat . toList) stepNbrhd
  in if null catSteps then Nothing else pure $ Step (origin n) catSteps

moves :: Board t => t BoardField -> BoardFigure -> Maybe Move
moves b bf
  | null capt == False = pure $ Left $ capt
  | isJust steps       = pure $ Right $ fromJust steps
  | otherwise          = Nothing
  where nbrs = neighborhood b bf (figureRange b (figureType bf))
        capt = (catMaybes . toList) $ capture (size b) bf <$> nbrs
        steps = step (size b) nbrs
