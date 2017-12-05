module Players.TestB (processMove) where
  
  import Types
  import Util

  afterFirst = doMove initState $ FromToMove (1,0) (3,0)

  processMove :: GameState -> IO Move
  processMove afterFirst = return $ FromToMove (6,1) (4,1)
  processMove _ = return Forfeit