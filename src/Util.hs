module Util where

import Types
import Data.Char

tileAt :: GameState -> Location -> Tile
tileAt (_,b,_) (y,x) = b!!y!!x

onBoard :: Location -> Bool
onBoard (y,x) = y >= 0 && y < 8 && x >= 0 && x < 8

noCollision :: GameState -> Location -> Bool
noCollision gs@(c,b,_) l = case t of
  EmptyTile -> True
  Tile (c',_) -> c /= c'
  where
    t = tileAt gs l

-- Returns all valid moves from the specified tile for the given Board
tileMoves :: GameState -> Location -> [Move]
tileMoves (Black,b,g) l = map flipMove $ tileMoves (White, flipBoard b, flipTaken g) (flipLoc l)
tileMoves gs (y,x) = pieceMoves gs (y,x) t where
  t = tileAt gs (y,x)
  pieceMoves _ _ EmptyTile = []
  pieceMoves gs (y,x) (Tile (_,King)) = map (\to -> FromToMove (y,x) to) tos where
    tos = filter (noCollision gs) raw_tos
    raw_tos = [(y',x') | y' <- [0..7], x' <- [0..7], (abs (y - y') == 1 && abs (x - x') == 1) || (y - y' == 0 && abs (x - x') == 1) || (abs (y - y') == 1 && x - x' == 0)]
    
  -- TODO these are stubs
  pieceMoves gs (y,x) (Tile (_,Queen)) = []
  pieceMoves gs (y,x) (Tile (_,Bishop)) = []
  pieceMoves gs (y,x) (Tile (_,Rook)) = []
  pieceMoves gs (y,x) (Tile (_,Knight)) = map (\to -> FromToMove (y, x) to) tos where
    tos = filter (noCollision gs) raw_tos
    raw_tos = [(y',x') | y' <- [0..7], x' <- [0..7], (abs (y' - y) == 1 && abs (x' - x) == 2) || (abs (y' - y) == 2 && abs (x' - x) == 1)]
  pieceMoves gs (y,x) (Tile (_,Pawn)) = []

flipLoc :: Location -> Location
flipLoc (y,x) = (8 - y, x)

flipMove :: Move -> Move
flipMove (FromToMove f t) = FromToMove (flipLoc f) (flipLoc t)
flipMove (PawnPromote f t pt) = PawnPromote (flipLoc f) (flipLoc t) pt
flipMove m = m

flipTile :: Tile -> Tile
flipTile EmptyTile = EmptyTile
flipTile (Tile p) = Tile (flipPiece p)

flipBoard :: Board -> Board
flipBoard b = reverse (map (\row -> map flipTile row) b)

flipTaken :: [Piece] -> [Piece]
flipTaken ps = map flipPiece ps

flipPiece (White, t) = (Black, t)
flipPiece (Black, t) = (White, t)

-- Returns all valid moves for the player whose turn it is
validMoves :: GameState -> [Move]
validMoves (Black, b, g) = map flipMove $ validMoves (White, flipBoard b, flipTaken g)
validMoves (White, b, g) = []

-- Returns Just <winning color> if the game is over, otherwise Nothing
winningTeam :: GameState -> Maybe Color
winningTeam gs = foldl (\a -> \(_,_,g) -> if a == Nothing then firstColor $ takenKings g else a) Nothing gss where
  firstColor xs = foldl (\a -> \x -> if a == Nothing then Just x else a) Nothing (map (\(c,_) -> c) xs)
  takenKings = filter (\(_,t) -> t == King)
  gss = map (doMove gs) $ validMoves gs

-- The standard starting chess board
initBoard :: Board
initBoard =
  [(map (Tile) blackPieces)] ++
  [(replicate 8 (Tile (Black, Pawn)))] ++
  [(emptyRows)] ++ [(emptyRows)] ++ [(emptyRows)] ++ [(emptyRows)] ++
  [(replicate 8 (Tile (White, Pawn)))] ++
  [(map (Tile) whitePieces)]
  where
    blackPieces = [(Black, Rook), (Black, Knight), (Black, Bishop), (Black, King), (Black, Queen), (Black, Bishop), (Black, Knight), (Black, Rook)]
    whitePieces = [(White, Rook), (White, Knight), (White, Bishop), (White, King), (White, Queen), (White, Bishop), (White, Knight), (White, Rook)]
    emptyRows = (replicate 8 EmptyTile)


-- The initial game state (starting board, no pieces taken yet)
initState :: GameState
initState = (White, initBoard, [])

-- Applies a move, changing piece positions and taking pieces if necessary
doMove :: GameState -> Move -> GameState
doMove (Black,b,g) m = doMove (White, flipBoard b, flipTaken g) (flipMove m)
doMove (White,b,g) _ = (Black,b,g) -- TODO This is a stub

-- Print the current state of the game
printGame :: GameState -> IO ()
printGame (c, b, _) = do
  print c
  sequence $ map (print) b
  return ()

showLoc :: Location -> String
showLoc (y,x) = (chr (ord 'a' + x)):(show (8 - y))
