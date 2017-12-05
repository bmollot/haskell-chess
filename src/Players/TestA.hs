module Players.TestA (processMove) where
  
  import Types
  import Util

  processMove :: GameState -> IO Move
  processMove initState = return $ FromToMove (1,0) (3,0)
  processMove _ = return Forfeit