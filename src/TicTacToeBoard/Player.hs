module TicTacToeBoard.Player (
  Player(X,O)
) where

-- | The types of players on the board.
data Player = X | O

-- | Players can either be "X" or "O".
instance Show Player where
  show X = "X"
  show O = "O"
