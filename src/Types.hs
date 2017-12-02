module Types where

data PieceType = King | Queen | Bishop | Rook | Knight | Pawn deriving Eq
instance Show PieceType where
  show King = "K"
  show Queen = "Q"
  show Bishop = "B"
  show Rook = "R"
  show Knight = "N" -- Not at all confusing
  show Pawn = "P"

data Color = White | Black deriving Eq
instance Show Color where
  show White = "White"
  show Black = "Black"

data Direction = North | South | West | East | NorthEast | NorthWest | SouthEast | SouthWest

data Tile = EmptyTile | Tile Piece deriving Eq
instance Show Tile where
  show EmptyTile = "  "
  show (Tile p) = showPiece p

data Move = FromToMove Location Location | PawnPromote Location Location PieceType | LeftCastle | RightCastle | OfferDraw | Forfeit
  deriving (Eq, Show)

type Piece = (Color, PieceType)
showPiece :: Piece -> String
showPiece (White, King) = "♔ "
showPiece (White, Queen) = "♕ "
showPiece (White, Rook) = "♖ "
showPiece (White, Bishop) = "♗ "
showPiece (White, Knight) = "♘ "
showPiece (White, Pawn) = "♙ "
showPiece (Black, King) = "♚ "
showPiece (Black, Queen) = "♛ "
showPiece (Black, Rook) = "♜ "
showPiece (Black, Bishop) = "♝ "
showPiece (Black, Knight) = "♞ "
showPiece (Black, Pawn) = "♟ "

type Location = (Int, Int)
type Board = [[Tile]] -- 8 x 8
-- Current turn, current board, taken pieces, ((Player 1 Left Castle Legal, P1 RC legal), (P2 LC legal, P2 RC legal))
type GameState = (Color, Board, [Piece], ((Bool, Bool), (Bool, Bool)))
type Player = (String, GameState -> IO Move)
