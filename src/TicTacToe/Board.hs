module TicTacToe.Board (
-- * Board types
  Board,
  BoardSquare,
-- * Board functions
  newBoard,
  pieceAt,
  insert,
  isValidMove
) where

import TicTacToe.Player

data Board = Board [[BoardSquare]]

type BoardSquare = Maybe Player

instance Show Board where
  show board = (unlines . map showRow) $ boardToList board
    where showRow                  = concatMap showBoardSquare
          showBoardSquare          = maybe "." show

-- |Initializes a new, empty board.
newBoard :: Board
newBoard = Board $ replicate boardSize $ replicate boardSize Nothing
  where boardSize = 3

insert :: Player -> Int -> Board -> Board
insert p idx board
  | isValidMove idx board = insert' p idx board
  | otherwise = board

insert' :: Player -> Int -> Board -> Board
insert' p idx board
  | idx `elem` [1..9] = Board $ prefixRows ++ [insertToRow $ bl !! rowZeroIndex] ++ suffixRows
  | otherwise = board
  where insertToRow row = concat [prefixCells row, [Just p], suffixCells row]
        columnIndex = (idx - 1) `mod` 3
        rowZeroIndex = (idx - 1) `div` 3
        splitRow = splitAt columnIndex
        bl = boardToList board
        prefixCells r = take (length (fst $ splitRow r)) (fst $ splitRow r)
        suffixCells r = drop 1 (snd $ splitRow r)
        prefixRows = take rowZeroIndex bl
        suffixRows = drop (rowZeroIndex + 1) bl

isValidMove :: Int -> Board -> Bool
isValidMove i b = pieceAt i b == Just Nothing

-- |Extracts the piece at the given index, assuming 1..9 indices.
pieceAt :: Int -> Board -> Maybe BoardSquare
pieceAt idx board
  | not $ isValidBoardIndex idx = Nothing
  | otherwise = Just $ concat (boardToList board) !! (idx - 1)

-- |Converts a board to a list.
boardToList :: Board -> [[BoardSquare]]
boardToList (Board b) = b

isValidBoardIndex :: Int -> Bool
isValidBoardIndex x = x `elem` [1..9]
