module TicTacToeBoard (
	Board,
  initBoard
) where

import TicTacToeBoard.Player

data Board = Board [[BoardSquare]]

type BoardSquare = Maybe Player

instance Show Board where
  show board = (unlines . map showRow) $ boardToList board
    where boardToList (Board b)    = b
          showRow                  = concatMap showBoardSquare
          showBoardSquare          = maybe "." show

initBoard :: Board
initBoard = Board $ replicate boardSize $ replicate boardSize Nothing
  where boardSize = 3

{-
spanM :: (Monad m) => (a -> Bool) -> [m a] -> m ([a], [m a])
spanM _ [] = return ([], [])
spanM p (a:as) = do
	x <- a
	if p x then do
		        (xs,bs) <- spanM p as
		        return (x:xs, bs)
		   else return ([x], as)
-}
