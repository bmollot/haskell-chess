# haskell-chess
Chess implemented as a Haskell program.

# TODO
## Types
- PieceType (King | Queen | Rook | etc)
- Color (White | Black)
- Piece (PieceType, Color)
- Tile (Empty | Piece)
- Location (Int, Int)
- Board ([Tile]) -- 8x8
- Move (Location, Location, Maybe PieceType) -- From, to, and type of piece to become (if move is a pawn reaching opposing side)
- GameState (Board, [Piece]) -- Current board, list of taken pieces

## Functions
- tileMoves :: Board -> Tile -> [Move] -- Returns all valid moves from the specified tile for the given Board
- validMoves :: Board -> Color -> [Move] -- Returns all valid moves for all pieces of the specified color
- winningTeam :: Board -> Maybe Color -- Returns Just <winning color> if the game is over, otherwise None
- initBoard :: Board -- The standard starting chess board
- initState = (initBoard, []) -- The initial game state (starting board, no pieces taken yet)
- doMove :: GameState -> Move -> GameState -- Applies a move, changing piece positions and taking pieces if necessary

