module Players.Human (processMove) where

import Types
import Util
import Data.Char

processMove :: GameState -> IO Move
processMove gs = do
  putStrLn "Enter move in the format: PieceCharFromFileFromRank_ToFileToRank :> "
  input <- getMove
  let piece = toUpper (input!!0)
  let fromFile = input!!1
  let fromFileInt = charToInt fromFile
  let fromRank = digitToInt (input!!2)
  let toFile = input!!4
  let toFileInt = charToInt toFile
  let toRank = digitToInt (input!!5)
  let from = (fromRank-1, fromFileInt)
  let to = (toRank-1, toFileInt)

  if (not(piece `elem` "KQNRBP") || fromFile < 'a' || fromFile > 'h' || toFile < 'a' || fromFile > 'h' || fromRank < 1 || fromRank > 8 ||
     toRank < 1 || toRank > 8) then putStrLn "Wrong input format, enter again :> " >> processMove gs
    else if (length from == 8 && PawnPromote from to (charToPieceType (toUpper (input!!7))) `elem` validMoves gs) then
           do return (PawnPromote from to (charToPieceType (toUpper (input!!7))))
         else if (FromToMove from to `elem` validMoves gs) then do return (FromToMove from to)
              else putStrLn "Invalid move entered, enter a valid move :> " >> processMove gs
  --else return (FromToMove (3,2) (3,3))

  
 -- return (FromToMove (3,2) (3,3))

getMove :: IO [Char]
getMove = do
  from <- getLine
  return from

charToInt :: Char -> Int
charToInt file
  |file == 'a' = 0
  |file == 'b' = 1
  |file == 'c' = 2
  |file == 'd' = 3
  |file == 'e' = 4
  |file == 'f' = 5
  |file == 'g' = 6
  |file == 'h' = 7

charToPieceType :: Char -> PieceType
charToPieceType pieceType
  |pieceType == 'K' = King
  |pieceType == 'Q' = Queen
  |pieceType == 'R' = Rook
  |pieceType == 'B' = Bishop
  |pieceType == 'N' = Knight
  |pieceType == 'P' = Pawn
