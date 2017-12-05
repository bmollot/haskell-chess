module Main where

import System.Environment

import Types
import Util
import qualified Players.Human as Hum
import qualified Players.TestA as TestA
import qualified Players.TestB as TestB

main :: IO ()
main = do
  args <- getArgs
  if args == ["test"] then runTest else runHumanGame

runHumanGame :: IO ()
runHumanGame = do
  putStrLn "Enter Player 1's name: "
  player1_name <- getLine
  putStrLn "Enter Player 2's name: "
  player2_name <- getLine
  putStrLn "\ninitial board setup "
  printGame initState
  play (player1_name, Hum.processMove) (player2_name, Hum.processMove) initState
  putStrLn ("\nGoodBye")

runTest :: IO ()
runTest = do
  putStrLn "Running tests..."
  play ("TestA", TestA.processMove) ("TestB", TestB.processMove) initState


play :: Player -> Player -> GameState -> IO ()
play player1@(p1_name,func) player2 gs = do
  move <- func gs
  let newState = doMove gs move
  putStrLn ("\n board after " ++ p1_name ++ "'s move ")
  printGame newState
  case winningTeam newState of
    Nothing -> play player2 player1 newState
    Just White -> putStrLn "Player2 won!"
    Just Black -> putStrLn "Player1 won!"
  --printGame newState
  --play player1 player2 newState
--(player1, initState)
--printGame initState
