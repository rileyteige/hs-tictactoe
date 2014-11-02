module TicTacToe.Board (
-- * Board types
  Board,
  BoardSquare,
-- * Board functions
  newBoard,
  fromList,
  pieceAt,
  insert,
  isValidMove,
  hasFullRow,
  hasFullColumn,
  hasFullDiagonal,
  hasStandardWin,
  hasStandardDraw
) where

import Data.List(transpose)
import Data.Maybe(isNothing, isJust)

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

-- |Initializes a new board from a list of squares.
fromList :: [[BoardSquare]] -> Board
fromList squares
  | length squares == 3 && all (\r -> length r == 3) squares = Board squares
  | otherwise = newBoard

hasStandardDraw :: Board -> Bool
hasStandardDraw b = isFull b && not (hasStandardWin b)

hasStandardWin :: Board -> Bool
hasStandardWin b = hasFullRow b || hasFullColumn b || hasFullDiagonal b

hasFullRow :: Board -> Bool
hasFullRow (Board b) = any allEqualOccupied b

hasFullColumn :: Board -> Bool
hasFullColumn (Board b) = any allEqualOccupied $ transpose b

hasFullDiagonal :: Board -> Bool
hasFullDiagonal b = any allEqualOccupied [leftDiagonal, rightDiagonal]
  where leftDiagonal  = piecesAt [1,5,9] b
        rightDiagonal = piecesAt [3,5,7] b

-- |Inserts a player's move into the board.
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
  | otherwise = Just $ pieceAt' idx board

pieceAt' :: Int -> Board -> BoardSquare
pieceAt' i b = concat (boardToList b) !! (i - 1)

piecesAt :: [Int] -> Board -> [BoardSquare]
piecesAt xs b = map (`pieceAt'` b) xs

-- |Converts a board to a list.
boardToList :: Board -> [[BoardSquare]]
boardToList (Board b) = b

isValidBoardIndex :: Int -> Bool
isValidBoardIndex x = x `elem` [1..9]

isFull :: Board -> Bool
isFull (Board bs) = all allOccupied bs

allOccupied :: [BoardSquare] -> Bool
allOccupied = all isJust

allEqualOccupied :: [BoardSquare] -> Bool
allEqualOccupied xs
  | isNothing (head xs) = False
  | otherwise = all (head xs ==) xs
