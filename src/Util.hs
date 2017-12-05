module Util where

import Types
import Data.Char
import qualified Data.Vector as V

tileAt :: GameState -> Location -> Tile
tileAt (_,b,_,_) (y,x) = b!!y!!x

onBoard :: Location -> Bool
onBoard (y,x) = y >= 0 && y < 8 && x >= 0 && x < 8

noCollision :: GameState -> Location -> Bool
noCollision gs@(c,b,_,_) l = case t of
  EmptyTile -> True
  Tile (c',_) -> c /= c'
  where
    t = tileAt gs l

canJumpTil :: GameState -> Location -> Direction -> Location
canJumpTil gs (y, x) dir =
  case dir of
    North -> if null northLst then (7, x) else (let hd = head northLst in (if (noCollision gs hd) then hd else ((fst hd)-1, x)))
    South -> if null southLst then (0,x) else (let last_elem = last southLst in
                                                 (if (noCollision gs last_elem) then last_elem else ((fst last_elem)+1, x)))


    East -> if null eastLst then (y, 7) else (let hd = head eastLst in (if (noCollision gs hd) then hd else (y, (snd hd)-1)))
    West -> if null westLst then (y,0) else (let last_elem = last westLst in
                                               (if (noCollision gs last_elem) then last_elem else (y, (snd last_elem)+1)))
    NorthEast -> if null nEstLst then (7,7) else (let hd = head nEstLst in (if (noCollision gs hd) then hd else ((fst hd)-1, (snd hd) -1)))
    NorthWest -> if null nWstLst then (7,0) else (let hd = head nWstLst in (if (noCollision gs hd) then hd else ((fst hd)-1, (snd hd)+1)))
    SouthEast -> if null sEstLst then (0,7) else (let last_elem = last sEstLst in
                                                    (if (noCollision gs last_elem) then last_elem else ((fst last_elem)+1, (snd last_elem)-1)))
    SouthWest -> if null sWstLst then (0,0) else (let last_elem = last sWstLst in
                                                    (if (noCollision gs last_elem) then last_elem else ((fst last_elem)+1, (snd last_elem)+1)))
    where
      northLst = [(y1,x1) | y1 <- [y+1..7], x1 <- [x], tileAt gs (y1,x1) /= EmptyTile]
      southLst = [(y1, x1) | y1 <- [0..y-1], x1 <- [x], tileAt gs (y1, x1) /= EmptyTile]
      eastLst =  [(y1, x1) | y1 <- [y], x1 <- [x+1..7], tileAt gs (y1, x1) /= EmptyTile]
      westLst = [(y1, x1) | y1 <- [y], x1 <- [0..x-1], tileAt gs (y1, x1) /= EmptyTile]
      nEstLst = [(y1, x1) | y1 <- [y+1..7], x1 <- [x+1..7], abs (y1 - y) == abs (x1 - x), tileAt gs (y1, x1) /= EmptyTile]
      nWstLst = [(y1, x1) | y1 <- [y+1..7], x1 <- [0..x-1], abs (y1 - y) == abs (x1 - x), tileAt gs (y1, x1) /= EmptyTile]
      sEstLst = [(y1, x1) | y1 <- [0..y-1], x1 <- [x+1..7], abs (y1 - y) == abs (x1 - x), tileAt gs (y1, x1) /= EmptyTile]
      sWstLst = [(y1, x1) | y1 <- [0..y-1], x1 <- [0..x-1], abs (y1 - y) == abs (x1 - x), tileAt gs (y1, x1) /= EmptyTile]

-- Returns all valid moves from the specified tile for the given Board
tileMoves :: GameState -> Location -> [Move]
tileMoves gs@(Black,_,_,_) l = map flipMove $ tileMoves (flipGame gs) (flipLoc l)
tileMoves gs (y,x) = pieceMoves gs (y,x) t where
  t = tileAt gs (y,x)
  pieceMoves _ _ EmptyTile = []
  
  pieceMoves gs (y,x) (Tile (_,King)) = map (\to -> FromToMove (y,x) to) tos where
    tos = filter (noCollision gs) raw_tos
    raw_tos = [(y',x') | y' <- [0..7], x' <- [0..7], (abs (y - y') == 1 && abs (x - x') == 1) || (y - y' == 0 && abs (x - x') == 1) || (abs (y - y') == 1 && x - x' == 0)]
    
  -- TODO these are stubs
  pieceMoves gs (y,x) (Tile (_,Queen)) = (pieceMoves gs (y,x) (Tile (White,Bishop))) ++ (pieceMoves gs (y,x) (Tile (White,King)))
  
  pieceMoves gs (y,x) (Tile (_,Bishop)) = (map (\to -> FromToMove (y,x) to) raw_tos_nEst) ++
                                          (map (\to -> FromToMove (y,x) to) raw_tos_nWst) ++
                                          (map (\to -> FromToMove (y,x) to) raw_tos_sEst) ++
                                          (map (\to -> FromToMove (y,x) to) raw_tos_sWst) where
    raw_tos_nEst = [(y',x') | y' <- [(y+1)..(fst (canJumpTil gs (y,x) NorthEast))], x' <- [(x+1)..(snd (canJumpTil gs (y,x) NorthEast))],
                   abs (y' - y) == abs (x' - x)]
    raw_tos_nWst = [(y',x') | y' <- [(y+1)..(fst (canJumpTil gs (y,x) NorthWest))], x' <- [(snd (canJumpTil gs (y,x) NorthWest))..(x-1)],
                   abs (y' - y) == abs (x' - x)]
    raw_tos_sEst = [(y',x') | y' <- [(fst (canJumpTil gs (y,x) SouthEast))..(y-1)], x' <- [(x+1)..(snd (canJumpTil gs (y,x) SouthEast))],
                   abs (y' - y) == abs (x' - x)]
    raw_tos_sWst = [(y',x') | y' <- [(fst (canJumpTil gs (y,x) SouthWest))..(y-1)], x' <- [(snd (canJumpTil gs (y,x) SouthWest))..(x-1)],
                   abs (y' - y) == abs (x' - x)]


                   
  pieceMoves gs (y,x) (Tile (_,Rook)) = (map (\to -> FromToMove (y,x) to) raw_tos_north) ++
                                        (map (\to -> FromToMove (y,x) to) raw_tos_south) ++
                                        (map (\to -> FromToMove (y,x) to) raw_tos_east) ++
                                        (map (\to -> FromToMove (y,x) to) raw_tos_west) where
    raw_tos_north = [(y', x) | y' <- [(y+1)..(fst (canJumpTil gs (y,x) North))]]
    raw_tos_south = [(y', x) | y' <- [(fst (canJumpTil gs (y,x) South))..(y-1)]]
    raw_tos_east = [(y, x') | x' <- [(x+1)..(snd (canJumpTil gs (y,x) East))]]
    raw_tos_west = [(y, x') | x' <- [(snd (canJumpTil gs (y,x) West))..(x-1)]]
                      
  pieceMoves gs (y,x) (Tile (_,Knight)) = map (\to -> FromToMove (y, x) to) tos where
    tos = filter (noCollision gs) raw_tos
    raw_tos = [(y',x') | y' <- [0..7], x' <- [0..7], (abs (y' - y) == 1 && abs (x' - x) == 2) || (abs (y' - y) == 2 && abs (x' - x) == 1)]
    
  pieceMoves gs (y,x) (Tile (_,Pawn)) = if (y < 6) then map (\to -> FromToMove (y,x) to) finalLst else if (y == 6) then promoteLst else [] where


    leftDiagonal = [(y+1, x-1) | x-1 >= 0, tileAt gs (y+1, x-1) /= EmptyTile, noCollision gs (y+1, x-1)]
    rightDiagonal = [(y+1, x+1) | x+1 <= 7, tileAt gs (y+1, x+1) /= EmptyTile, noCollision gs (y+1,x+1)]
    northLst = if (y == 1) then getLst else oneUpLst
    getLst = if tileAt gs (y+1,x) == EmptyTile && (noCollision gs (y+2,x)) then [(y+1,x),(y+2,x)] else oneUpLst
    oneUpLst = if ((tileAt gs (y+1,x)) == EmptyTile) then [(y+1,x)] else []
    finalLst = leftDiagonal ++ rightDiagonal ++ northLst
    promoteLst = [PawnPromote (y,x) to piece | to <- finalLst, piece <- [Queen, Knight, Rook, Bishop]]

flipLoc :: Location -> Location
flipLoc (y,x) = (7 - y, x)

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

flipPiece :: Piece -> Piece
flipPiece (White, t) = (Black, t)
flipPiece (Black, t) = (White, t)

flipGame :: GameState -> GameState
flipGame (c, b, t, (b1, b2)) = (if c == White then Black else White, flipBoard b, flipTaken t, (b2, b1))

-- Returns all valid moves for the player whose turn it is
validMoves :: GameState -> [Move]
validMoves gs@(Black,_,_,((p1lc, p1rc),(p2lc,p2rc))) = map flipMove $ validMoves $ flipGame gs
validMoves gs = resultLst where
  pawnMoves = [moves | x <- [0..7], y <- [0..7], tileAt gs (x,y) == Tile (White, Pawn), moves <- tileMoves gs (x,y)]
  rookMoves = [moves | x <- [0..7], y <- [0..7], tileAt gs (x,y) == Tile (White, Rook), moves <- tileMoves gs (x,y)]
  knightMoves = [moves | x <- [0..7], y <- [0..7], tileAt gs (x,y) == Tile (White, Knight), moves <- tileMoves gs (x,y)]
  bishopMoves = [moves | x <- [0..7], y <- [0..7], tileAt gs (x,y) == Tile (White, Bishop), moves <- tileMoves gs (x,y)]
  queenMoves = [moves | x <- [0..7], y <- [0..7], tileAt gs (x,y) == Tile (White, Queen), moves <- tileMoves gs (x,y)]
  kingMoves = [moves | x <- [0..7], y <- [0..7], tileAt gs (x,y) == Tile (White, King), moves <- tileMoves gs (x,y)]
  resultLst = pawnMoves ++ rookMoves ++ knightMoves ++ bishopMoves ++ queenMoves ++ kingMoves 

validCastleMoves :: GameState -> [Move]
validCastleMoves gs@(Black,b,_,((p1lc, p1rc),(p2lc,p2rc))) = resultLst where
  lcEmpty = ((tileAt gs (7,1) == EmptyTile) && (tileAt gs (7,2) == EmptyTile) && (tileAt gs (7,3) == EmptyTile))
  rcEmpty = ((tileAt gs (7,5) == EmptyTile) && (tileAt gs (7,6) == EmptyTile))
  opponentMoves = validMoves (White, b, [], ((p1lc,p1rc), (p2lc, p2rc)))
  leftClear = foldl (\a x -> if (to (7,3) x) == False then False else a) True opponentMoves
  rightClear = foldl (\a x -> if (to (7,5) x) == False then False else a) True opponentMoves
  leftCastle = if (p1lc == True && lcEmpty && leftClear) then [LeftCastle] else []
  rightCastle = if (p1rc == True && rcEmpty && rightClear) then [RightCastle] else []
  resultLst = leftCastle ++ rightCastle
  to (y1,x1) move =
    case move of
      FromToMove from (y',x') -> if (((y' == y1) && (x' == x1)) || ((y' == 7) && (x' == 4))) then False else True
      PawnPromote from (y',x') piece -> if (((y' == y1) && (x' == x1)) || ((y' == 7) && (x' == 4))) then False else True

validCastleMoves gs@(_,b,_,((p1lc,p1rc),(p2lc,p2rc))) = resultLst where
  lcEmpty = ((tileAt gs (0,1) == EmptyTile) && (tileAt gs (0,2) == EmptyTile) && (tileAt gs (0,3) == EmptyTile))
  rcEmpty = ((tileAt gs (0,5) == EmptyTile) && (tileAt gs (0,6) == EmptyTile))
  opponentMoves = validMoves (White, flipBoard b, [], ((p1lc,p1rc), (p2lc, p2rc)))
  leftClear = foldl (\a x -> if (to (7,3) x) == False then False else a) True opponentMoves
  rightClear = foldl (\a x -> if (to (7,5) x) == False then False else a) True opponentMoves
  leftCastle = if (p1lc == True && lcEmpty && leftClear) then [LeftCastle] else []
  rightCastle = if (p1rc == True && rcEmpty && rightClear) then [RightCastle] else []
  resultLst = leftCastle ++ rightCastle
  to (y1,x1) move =
    case move of
      FromToMove from (y',x') -> if (((y' == y1) && (x' == x1)) || ((y' == 7) && (x' == 4))) then False else True
      PawnPromote from (y',x') piece -> if (((y' == y1) && (x' == x1)) || ((y' == 7) && (x' == 4))) then False else True
      
-- Returns Just <winning color> if the game is over, otherwise Nothing
winningTeam :: GameState -> Maybe Color
winningTeam gs = foldl (\a -> \(_,_,g,_) -> if a == Nothing then firstColor $ takenKings g else a) Nothing gss where
  firstColor xs = foldl (\a -> \x -> if a == Nothing then Just x else a) Nothing (map (\(c,_) -> c) xs)
  takenKings = filter (\(_,t) -> t == King)
  gss = map (doMove gs) $ validMoves gs

-- The standard starting chess board
initBoard :: Board
initBoard =
  [(map (Tile) whitePieces)] ++ [(replicate 8 (Tile (White, Pawn)))] ++
  [(emptyRows)] ++ [(emptyRows)] ++ [(emptyRows)] ++ [(emptyRows)] ++
  [(replicate 8 (Tile (Black, Pawn)))] ++ [(map (Tile) blackPieces)]
  where
    blackPieces = [(Black, Rook), (Black, Knight), (Black, Bishop), (Black, Queen), (Black, King), (Black, Bishop), (Black, Knight), (Black, Rook)]
    whitePieces = [(White, Rook), (White, Knight), (White, Bishop), (White, Queen), (White, King), (White, Bishop), (White, Knight), (White, Rook)]
    emptyRows = (replicate 8 EmptyTile)


-- The initial game state (starting board, no pieces taken yet)
initState :: GameState
initState = (White, initBoard, [], ((True, True), (True, True)))

-- Applies a move, changing piece positions and taking pieces if necessary
doMove :: GameState -> Move -> GameState
doMove gs@(Black,_,_,_) m = flipGame $ doMove (flipGame gs) (flipMove m)
doMove gs@(White,b,g,((p1lc, p1rc), (p2lc, p2rc))) (FromToMove from@(y,x) to@(y', x')) =
  (Black, resulting_board, updated_taken_lst, ((p1lc', p1rc'), (p2lc', p2rc'))) where
    updated_taken_lst = if to_tile == EmptyTile then g else let (Tile x) = to_tile in (x:g)
    to_tile = (tileAt gs to)
    outerVector = V.fromList b
    from_Vector = V.fromList (b!!y)
    tile_piece = (tileAt gs from)
    to_Vector = V.fromList (b!!(fst to))
    update_toVector = to_Vector V.// [((snd to), tile_piece)]
    update_fromVector = from_Vector V.// [(x, EmptyTile)]
    outer_update1 = outerVector V.// [(y, V.toList update_fromVector)]
    final_updated_vector = outer_update1 V.// [((fst to), V.toList update_toVector)]
    resulting_board = V.toList final_updated_vector
    p1lc' = not ((y == 0) && (x == 0 || x == 4)) && p1lc
    p1rc' = not ((y == 0) && (x == 7 || x == 4)) && p1rc
    p2lc' = not ((y' == 0) && (x' == 0 || x' == 4)) && p2lc
    p2rc' = not ((y' == 0) && (x' == 7 || x' == 4)) && p2rc

doMove gs@(White,b,g,((p1lc, p1rc), (p2lc, p2rc))) (PawnPromote from@(y,x) to@(y', x') piece) =
  (Black, resulting_board, updated_taken_lst, ((p1lc', p1rc'), (p2lc', p2rc'))) where
    updated_taken_lst = if to_tile == EmptyTile then g else let (Tile x) = to_tile in (x:g)
    to_tile = (tileAt gs to)
    outerVector = V.fromList b
    from_Vector = V.fromList (b!!y)
    tile_piece = (tileAt gs from)
    to_Vector = V.fromList (b!!(fst to))
    update_toVector = to_Vector V.// [((snd to), Tile (White,piece))]
    update_fromVector = from_Vector V.// [(x, EmptyTile)]
    outer_update1 = outerVector V.// [(y, V.toList update_fromVector)]
    final_updated_vector = outer_update1 V.// [((fst to), V.toList update_toVector)]
    resulting_board = V.toList final_updated_vector
    p1lc' = not ((y == 0) && (x == 0 || x == 4)) && p1lc
    p1rc' = not ((y == 0) && (x == 7 || x == 4)) && p1rc
    p2lc' = not ((y' == 0) && (x' == 0 || x' == 4)) && p2lc
    p2rc' = not ((y' == 0) && (x' == 7 || x' == 4)) && p2rc

doMove gs@(White,b,g,((p1lc, p1rc), (p2lc, p2rc))) LeftCastle =
  (Black, resulting_board, g, ((False, p1rc), (p2lc, p2rc))) where
    --to_tile = (tileAt gs to)
    outerVector = V.fromList b
    from_Vector = V.fromList (b!!0)
    to_Vector = V.fromList (b!!0)
    update_Vector1 = to_Vector V.// [(2, Tile (White,King))]
    update_Vector2 = update_Vector1 V.// [(4, EmptyTile)]
    update_Vector3 = update_Vector2 V.// [(3, Tile (White,Rook))]
    update_Vector4 = update_Vector3 V.// [(0, EmptyTile)]
    outer_update1 = outerVector V.// [(0, V.toList update_Vector4)]
    --final_updated_vector = outer_update1 V.// [((fst to), V.toList update_toVector)]
    resulting_board = V.toList outer_update1

doMove gs@(White,b,g,((p1lc, p1rc), (p2lc, p2rc))) RightCastle =
  (Black, resulting_board, g, ((p1lc, False), (p2lc, p2rc))) where
    --to_tile = (tileAt gs to)
    outerVector = V.fromList b
    from_Vector = V.fromList (b!!0)
    to_Vector = V.fromList (b!!0)
    update_Vector1 = to_Vector V.// [(6, Tile (White,King))]
    update_Vector2 = update_Vector1 V.// [(4, EmptyTile)]
    update_Vector3 = update_Vector2 V.// [(5, Tile (White,Rook))]
    update_Vector4 = update_Vector3 V.// [(7, EmptyTile)]
    outer_update1 = outerVector V.// [(0, V.toList update_Vector4)]
    --final_updated_vector = outer_update1 V.// [((fst to), V.toList update_toVector)]
    resulting_board = V.toList outer_update1

doMove (White,b,g,c) Forfeit = (Black, b, (White,King):g, c)

doMove gs _ = gs


-- Print the current state of the game
printGame :: GameState -> IO ()
printGame (c, b, g, _) = do
  print c
  print g
  putStrLn "   ┌──┬──┬──┬──┬──┬──┬──┬──┐"
  sequence $ reverse (map (printRow) $ zip b [0..])
  putStrLn "   └──┴──┴──┴──┴──┴──┴──┴──┘"
  putStrLn "    a  b  c  d  e  f  g  h  "
  return ()
  where
    printRow (r,n) = do
      putStrLn $ concat [" ", show (n+1), " │", show $ r!!0, "│", show $ r!!1, "│", show $ r!!2, "│", show $ r!!3, "│", show $ r!!4, "│", show $ r!!5, "│", show $ r!!6, "│", show $ r!!7, "│"]
      if n == 0 then return () else putStrLn ("" ++ "   ├──┼──┼──┼──┼──┼──┼──┼──┤")

showLoc :: Location -> String
showLoc (y,x) = (chr (ord 'a' + x)):(show (7 - y))
