# haskell-chess
Chess implemented as a Haskell program.

Players are tuples of (String, GameState -> Move), where String is a String representing the player name, and GameState -> Move is a function that takes the current game state and returns the next move to make.
Place modules exporting a Player object in `src/players`.
In the future there will be script that scans for players and generates a binary allowing any two players to be chosen to compete.
