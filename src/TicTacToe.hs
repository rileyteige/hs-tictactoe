module TicTacToe (
  Board,
  BoardSquare,
  initBoard,
  convertBoard
) where

import TicTacToe.Board
import qualified TicTacToe.Internal as Internal

convertBoard :: Board -> Board
convertBoard = Internal.convertSquares
