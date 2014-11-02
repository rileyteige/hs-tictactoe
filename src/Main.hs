module Main (
  main
) where

import System.IO(hFlush, stdout)
import TicTacToe.Board
import TicTacToe.Player

data PlayState = Playing | Draw | Victory deriving (Eq, Show)
data GameState = GameState Board Player PlayState

instance Show GameState where
  show (GameState b p ps) = "(" ++ show b ++ ", " ++ show p ++ ", " ++ show ps ++ ")"

main :: IO()
main = playGame newBoard

playGame :: Board -> IO()
playGame board = do
  (GameState finalBoard winningPlayer playState) <- playTurns $ GameState board X Playing
  if playState == Victory then putStrLn $ show winningPlayer ++ " wins!"
                          else putStrLn "Draw."
  putStrLn "FINAL BOARD:"
  print finalBoard
  return ()

playTurns :: GameState -> IO GameState
playTurns (GameState board currentPlayer _)
  | isVictory = return $ GameState board nextPlayer Victory
  | isDraw    = return $ GameState board nextPlayer Draw
  | otherwise = do
      showCurrentState
      move <- readMove board
      playTurns $ GameState (insert currentPlayer move board) nextPlayer Playing
  where nextPlayer = if currentPlayer == X then O else X
        isVictory  = hasStandardWin board
        isDraw     = hasStandardDraw board
        showCurrentState = do
          putStrLn $ "\nCURRENT PLAYER: " ++ show currentPlayer
          putStrLn "CURRENT BOARD:"
          print board

readMove :: Board -> IO Int
readMove board = do
  moveStr <- prompt "Enter a row-biased 1-9 coordinate to play: "
  let move = readInt moveStr
  if move `elem` [1..9] && isValidMove move board
     then return move
     else readMove board

readInt :: String -> Int
readInt str = read str :: Int

prompt :: String -> IO String
prompt str = do
  putStr str
  hFlush stdout
  getLine
