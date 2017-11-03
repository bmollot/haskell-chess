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
  show White = "W"
  show Black = "B"

data Tile = EmptyTile | Tile Piece
instance Show Tile where
  show EmptyTile = "  "
  show (Tile (c, t)) = show c ++ show t

data Move = FromToMove Location Location | PawnPromote Location Location PieceType | LeftCastle | RightCastle | Forfeit

type Piece = (Color, PieceType)
type Location = (Int, Int)
type Board = [[Tile]] -- 8 x 8
-- Current turn, current board and taken pieces
type GameState = (Color, Board, [Piece])
type Player = (String, GameState -> Move)