module Main where

import Types
import Util
import Players.Human (processMove)

--player1 :: Player
--player2 :: Player

main :: IO ()
main = do
  putStrLn "Enter Player 1's name: "
  player1_name <- getLine
  putStrLn "Enter Player 2's name: "
  player2_name <- getLine
  putStrLn "\n initial board setup "
  printGame initState
  play (player1_name, processMove) (player2_name, processMove) initState
  putStrLn ("\nGoodBye")

play :: Player -> Player -> GameState -> IO ()
play player1@(p1_name,func) player2@(p2_name, func2) gs = do
  move <- processMove gs
  let newState = doMove gs move
  putStrLn ("\n board after " ++ p1_name ++ "'s move ")
  printGame newState
  case winningTeam newState of
    Nothing -> play player2 player1 newState
    Just White -> putStrLn "Player1 won!"
    Just Black -> putStrLn "Player2 won!"
  --printGame newState
  --play player1 player2 newState
--(player1, initState)
--printGame initState
